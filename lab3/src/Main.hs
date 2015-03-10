module Main (main) where

import System.Environment (getArgs)
import System.Console.Haskeline
import Control.Exception (AsyncException(..))
import System.Directory

import Monite.Interpret

import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT, get, modify, lift )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Control.Monad (forM, liftM)
import System.FilePath
import Data.List (isPrefixOf)

import qualified Data.Map as M

-- TODO: Fix file placement! : 2015-03-06 - 16:08:11 (John)
mySettings :: FilePath -> Settings IO
mySettings path = setComplete myComplete $
  defaultSettings { historyFile = Just $ path ++ "/monitehistory" }

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  env <- emptyEnv
  case args of
    [file] -> do s <- readFile file               -- read the script file
                 path <- getCurrentDirectory
                 run (interpret s) (Env M.empty path)            -- interpret the script file
                 return ()
    _      -> do setCurrentDirectory (path env)
                 inputLoop env                     -- get user commands

-- | Get commands from the user
inputLoop :: Env -> IO ()
inputLoop env = do
  path <- getHomeDirectory
  runInputT (mySettings path) $ withInterrupt $ loop env         -- input loop w/ default sets
  where
    loop :: Env -> InputT IO ()
    loop env = do
      let prompt = path env -- TODO: Makeup? : 2015-03-03 - 16:52:39 (John)
      minput <- handle (\Interrupt -> return $ Just "") (getInputLine ("λ> "))                 -- get user command
      case minput of
        Nothing -> return ()                      -- nothing entered
        Just "quit" -> return ()                  -- quit the shell loop
        Just input -> do env <- liftIO $ run (interpret input) env -- interpret user command
                         loop env

-- | Run the MoniteM monad, with a given environment
run :: MoniteM a -> Env -> IO a
run m env = do
  res <- runExceptT $ evalStateT (runMonite m) env
  case res of
    Left err -> fail $ "error"
    Right a  -> return a


-- | Custom completion function for hskeline, which will consider the first word
-- on the input line as a binary, and thus, suggest completions from the users
-- $PATH in the enviroment, otherwise it will be considered as a file, and will
-- suggest completions depending on the files in the current directory of
-- moniteshell.
myComplete :: (MonadIO m) => CompletionFunc m
myComplete line@(left, right)
  | isBinaryCommand left =
  completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars) listBinFiles $ line
  | otherwise =
  completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars) listFiles $ line

-- | Check if it is a binary command, i.e., the first word on the input line,
-- which can not be followed by a white space
isBinaryCommand :: String -> Bool
isBinaryCommand []  = True
isBinaryCommand cmd = l == 1 && (not b)
  where rcmd = reverse cmd
        b    = last rcmd == ' '
        l    = length (words rcmd)

-- | Given the input of the first word to be completed, it will return the
-- matching completions found in the users path.
listBinFiles :: (MonadIO m) => FilePath -> m [Completion]
listBinFiles path = liftIO $ do
  binPaths <- liftM (map (++ "/" ++ path)) getSearchPath -- TODO: Get path other way : 2015-03-10 - 13:03:29 (John)
  comps <- mapM listBinFiles' binPaths
  return (concat comps)

-- | List all of the files or folders beginning with this path.
listBinFiles' :: MonadIO m => FilePath -> m [Completion]
listBinFiles' path = liftIO $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    binFiles <- if not dirExists
                    then return []
                    else fmap (map completion . filterPrefix)
                            $ getDirectoryContents fixedDir

    let allFiles = binFiles
    -- The replacement text should include the directory part, and also
    -- have a trailing slash if it's itself a directory.
    forM allFiles $ \c -> do
            isDir <- doesDirectoryExist (fixedDir </> replacement c)
            return $ c -- setReplacement fullName $ alterIfDir isDir c
  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> notElem f [".",".."]
                                        && file `isPrefixOf` f)

-- turn a user-visible path into an internal version useable by System.FilePath.
fixPath :: String -> IO String
-- For versions of filepath < 1.2
fixPath "" = return "."
fixPath ('~':c:path) | isPathSeparator c = do
    home <- getHomeDirectory
    return (home </> path)
fixPath path = return path

completion :: String -> Completion
completion str = Completion str str False
