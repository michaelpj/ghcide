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
import           Data.Binary
import qualified Data.ByteString               as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
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

-- | A wrapper around a mutable 'FileExistsState'
newtype FileExistsMapVar = FileExistsMapVar (Var FileExistsMap)

instance IsIdeGlobal FileExistsMapVar

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  liftIO $ readVar v

{- Note [Invalidating file existence results]
We have two mechanisms for getting file existence information:
- The file existence cache
- The VFS lookup

Both of these affect the results of the 'GetFileExists' rule, so we need to make sure it
is invalidated properly when things change.

For the file existence cache, we manually flush the results of 'GetFileExists' when we
modify it (i.e. when a notification comes from the client).

For the VFS lookup, however, we won't get prompted to flush the result, so instead
we use 'alwaysRerun'.
-}

{- Note [Invalidating file existence results]
We have two mechanisms for getting file existence information:
- The file existence cache
- The VFS lookup

Both of these affect the results of the 'GetFileExists' rule, so we need to make sure it
is invalidated properly when things change.

For the file existence cache, we manually flush the results of 'GetFileExists' when we
modify it (i.e. when a notification comes from the client).

For the VFS lookup, however, we won't get prompted to flush the result, so instead
we use 'alwaysRerun'.
-}

-- | Modify the global store of file exists.
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ HashMap.fromList changes

  -- Masked to ensure that the previous values are flushed together with the map update
  mask $ \_ -> do
    -- update the map
    modifyVar_ var $ \mp -> evaluate $ HashMap.union changesMap mp
    -- flush previous values, see Note [Invalidating file existence results]
    mapM_ (deleteValue state GetFileExists . fst) changes

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
fileExistsRules :: ClientCapabilities -> VFSHandle -> Rules ()
fileExistsRules ClientCapabilities{_workspace} vfs = do
    -- Create the global always, although it should only be used if we have fast rules.
    -- But there's a chance someone will send unexpected notifications anyway,
    -- e.g. https://github.com/digital-asset/ghcide/issues/599
    addIdeGlobal . FileExistsMapVar =<< liftIO (newVar [])

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
        alwaysRerun
        fileExistsFast watchSupported vfs file

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

fileExistsFast :: Bool -> VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsFast True vfs file = do
    mp <- getFileExistsMapUntracked

    let mbFilesWatched = HashMap.lookup file mp
    exist <- case mbFilesWatched of
      Just exist -> pure exist
      -- We don't know about it: back to fileExistsSlow we go.
      -- We need to call it explicitly so we invalidate the rule result for this
      -- case correctly, see Note [Invalidating file existence results]
      Nothing -> liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))
fileExistsFast False vfs file = do
    exist <-liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton 1 else BS.empty

getFileExistsVFS :: VFSHandle -> NormalizedFilePath -> IO Bool
getFileExistsVFS vfs file = do
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    handle (\(_ :: IOException) -> return False) $
        (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
        Dir.doesFileExist (fromNormalizedFilePath file)
