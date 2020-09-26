{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  )
where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad.Extra
import qualified Data.Aeson                    as A
import           Data.Binary
import qualified Data.ByteString               as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Maybe
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import qualified System.Directory as Dir

{- Note [File existence cache and LSP file watchers]
Some LSP servers provide the ability to register file watches with the client, which will then notify
us of file changes. Some clients can do this more efficiently than us, or generally it's a tricky
problem

Here we use this to maintain a quick lookup cache of file existence. How this works is:
- On startup, if the client supports it we ask it to watch some files (see below).
- When those files are created or deleted (we can also see change events, but we don't
care since we're only caching existence here) we get a notification from the client.
- The notification handler calls 'modifyFileExists' to update our cache.

This means that the cache will only ever work for the files we have set up a watcher for.
So we pick the set that we mostly care about and which are likely to change existence
most often: the source files of the project (as determined by the source extensions
we're configured to care about).

For all other files we fall back to the slow path.
-}

-- See Note [File existence cache and LSP file watchers]
-- | A map for tracking the file existence.
-- If a path maps to 'True' then it exists; if it maps to 'False' then it doesn't exist'; and
-- if it's not in the map then we don't know.
type FileExistsMap = (HashMap NormalizedFilePath Bool)

-- | The state of our file existence cache. A pair of a boolean indicating whether we have initialized the
-- file watcher in the client yet, and a map tracking file existence.
data FileExistsState = FileExistsState { fileExistsWatcherInitialized :: Bool, fileExistsMap :: FileExistsMap }

-- | A wrapper around a mutable 'FileExistsState'
newtype FileExistsStateVar = FileExistsStateVar (Var FileExistsState)

instance IsIdeGlobal FileExistsStateVar

-- | Grab the current global value of 'FileExistsState' without acquiring a dependency
getFileExistsStateUntracked :: Action FileExistsState
getFileExistsStateUntracked = do
  FileExistsStateVar v <- getIdeGlobalAction
  liftIO $ readVar v

-- | Modify the global store of file exists
modifyFileExistsStateAction :: (FileExistsState -> IO FileExistsState) -> Action ()
modifyFileExistsStateAction f = do
  FileExistsStateVar var <- getIdeGlobalAction
  liftIO $ modifyVar_ var f

-- | Modify the global store of file exists.
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsStateVar var <- getIdeGlobalState state
  changesMap             <- evaluate $ HashMap.fromList changes
  -- Update the map. No need to invalidate anything, since the rules are 'alwaysRerun'
  modifyVar_ var $ \st -> evaluate $ st{fileExistsMap=HashMap.union changesMap (fileExistsMap st)}

-------------------------------------------------------------------------------------

type instance RuleResult GetFileExists = Bool

data GetFileExists = GetFileExists
    deriving (Eq, Show, Typeable, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists
instance Binary   GetFileExists

-- | Returns True if the file exists
--   Note that a file is not considered to exist unless it is saved to disk.
--   In particular, VFS existence is not enough.
--   Consider the following example:
--     1. The file @A.hs@ containing the line @import B@ is added to the files of interest
--        Since @B.hs@ is neither open nor exists, GetLocatedImports finds Nothing
--     2. The editor creates a new buffer @B.hs@
--        Unless the editor also sends a @DidChangeWatchedFile@ event, ghcide will not pick it up
--        Most editors, e.g. VSCode, only send the event when the file is saved to disk.
getFileExists :: NormalizedFilePath -> Action Bool
getFileExists fp = use_ GetFileExists fp

-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: IO LspId -> ClientCapabilities -> VFSHandle -> Rules ()
fileExistsRules getLspId ClientCapabilities{_workspace} vfs = do
    -- Create the global always, although it should only be used if we have fast rules.
    -- But there's a chance someone will send unexpected notifications anyway,
    -- e.g. https://github.com/digital-asset/ghcide/issues/599
    addIdeGlobal . FileExistsStateVar =<< liftIO (newVar (FileExistsState False []))

    let watchSupported = case () of
          _ | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
            , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
            , Just True <- _dynamicRegistration
              -> True
            | otherwise -> False

    unless watchSupported $ do
        logger <- logger <$> getShakeExtrasRules
        liftIO $ logDebug logger "Warning: Client does not support watched files. Falling back to OS polling"

    defineEarlyCutoff $ \GetFileExists file -> do
        -- We don't generally know when this has been invalidated if we had to do a VFS lookup. We *could*
        -- track invalidation better for cases that are tracked in the file existence cache, but the rule
        -- is cheap, just a hash map lookup, so always rerunning is good enough.
        alwaysRerun
        if watchSupported
            then fileExistsFast getLspId vfs file
            else fileExistsSlow vfs file

{- Note [Which files should we watch?]
The watcher system gives us a lot of flexibility: we can set multiple watchers, and they can all watch on glob
patterns.

We used to have a quite precise system, where we would register a watcher for a single file path only (and always)
when we actually looked to see if it existed. The downside of this is that it sends a *lot* of notifications
to the client (thousands on a large project), and this could lock up some clients like emacs
(https://github.com/emacs-lsp/lsp-mode/issues/2165).

Now we take the opposite approach: we register a single, quite general watcher that looks for all files
with a predefined set of extensions. The consequences are:
- The client will have to watch more files. This is usually not too bad, since the pattern is a single glob,
and the clients typically call out to an optimized implementation of file watching that understands globs.
- The client will send us a lot more notifications. This isn't too bad in practice, since although
we're watching a lot of files in principle, they don't get created or destroyed that often.
- We won't ever hit the fast lookup path for files which aren't in our watch pattern, since the only way
files get into our map is when the client sends us a notification about them because we're watching them.
This is fine so long as we're watching the files we check most often, i.e. source files.
-}

-- | Register the global file watcher. Requires an lsp client that provides WatchedFiles notifications, but
-- assumes that this has already been checked.
registerWatcher :: LspId -> ShakeExtras -> IdeOptions -> IO ()
registerWatcher lspId ide opts = do
    let
      req = RequestMessage "2.0" lspId ClientRegisterCapability regParams
      regParams    = RegistrationParams (List [registration])
      -- The registration ID is arbitrary and is only used in case we want to deregister (which we won't).
      -- We could also use something like a random UUID, as some other servers do, but this works for
      -- our purposes.
      registration = Registration "globalFileWatches"
                                  WorkspaceDidChangeWatchedFiles
                                  (Just (A.toJSON regOptions))
      regOptions =
        DidChangeWatchedFilesRegistrationOptions { _watchers = List [watcher] }
      exts = optExtensions opts
      -- See Note [File existence cache and LSP file watchers] for why this exists, and the choice of watch kind
      watchKind = WatchKind { _watchCreate = True, _watchChange = False, _watchDelete = True}
      -- See Note [What files should we watch?] for an explanation of why the pattern is the way that it is
      -- The pattern will be something like "**/.{hs,lhs}", i.e. "any number of directory segments,
      -- followed by a file with extension 'hs' or 'lhs'".
      watcher = FileSystemWatcher { _globPattern = "**/*.{" ++ intercalate "," exts ++ "}"
                                  , _kind        = Just watchKind
                                  }

    liftIO $ eventer ide $ ReqRegisterCapability req

fileExistsFast :: IO LspId -> VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsFast getLspId vfs file = do
    st <- getFileExistsStateUntracked

    -- If we have not yet registered the file watcher, then do so now.
    -- We do this on the first run of 'fileExistsFast' rather than in, say 'fileExistsRules' because
    -- we can't send any messages then: they'd be sent before we are finished initializing the server
    -- which is illegal. So we delay it to here.
    unless (fileExistsWatcherInitialized st) $ do
        ide <- getShakeExtras
        opts <- getIdeOptions
        -- Check again with the lock held: so we definitely only send the message once
        modifyFileExistsStateAction $ \st -> do
            unless (fileExistsWatcherInitialized st) $ do
                lspId <- getLspId
                registerWatcher lspId ide opts
            -- Either it was already true, in which case this does nothing,
            -- or we're setting it now
            pure $ st { fileExistsWatcherInitialized=True }

    let mbFilesWatched = HashMap.lookup file (fileExistsMap st)
    case mbFilesWatched of
      Just exist -> pure (summarizeExists exist, ([], Just exist))
      -- We don't know about it: back to fileExistsSlow we go.
      Nothing -> fileExistsSlow vfs file

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton 1 else BS.empty

fileExistsSlow :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsSlow vfs file = do
    exist <- liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

getFileExistsVFS :: VFSHandle -> NormalizedFilePath -> IO Bool
getFileExistsVFS vfs file = do
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    handle (\(_ :: IOException) -> return False) $
        (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
        Dir.doesFileExist (fromNormalizedFilePath file)
