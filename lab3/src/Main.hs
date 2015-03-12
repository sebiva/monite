module Main (main) where

import Monite.Interpret

import System.Environment (getArgs)
import System.Console.Haskeline
import System.Directory
import System.FilePath

import Control.Exception (AsyncException(..))
import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT, get, modify, lift )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad (forM, liftM)

import Data.List (isPrefixOf)
import qualified Data.Map as M

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  env <- initEnv
  case args of
    -- File mode
    [file] -> do
      s <- readFile file                     -- read the script file
      path <- getCurrentDirectory
      run (interpret s) (emptyEnv path) -- interpret the script file
      return ()
    -- Shell mode
    _      -> do
      setCurrentDirectory (path env)         -- defaults to $HOME
      inputLoop env                          -- get user commands

-- | Get commands from the user
inputLoop :: Env -> IO ()
inputLoop env = do
  -- Get settings
  settings <- mySettings
  -- Get preferences
  prefs    <- myPrefs
  -- Run the input loop
  runInputTWithPrefs prefs settings $ withInterrupt $ loop env

-- | The main loop of the program, interprets the user input.
loop :: Env -> InputT IO ()
loop env = do
  -- get user command
  home <- liftIO getHomeDirectory
  let prmpt = prompt (path env) home
  minput <- handleInterrupt (return $ Just "") (getInputLine (prmpt ++ " λ> "))
  case minput of
    Nothing    -> exitLoop
    Just input -> if isExitCode input then exitLoop else runLoop input
  where
    -- exit the loop
    exitLoop    = return ()
    -- interpert entered command, and loop
    runLoop inp = do env <- withInterrupt $ liftIO $ run (interpret inp) env
                     loop env

-- | Create a prompt path from the current working dir, shortening $HOME to
-- '~'. Also shortens all but the topmost directory to their first letter.
prompt :: FilePath -> FilePath -> String
prompt p home
 | take (length home) p == home = '~' : prompt' (drop (length home) p)
 | otherwise                    = prompt' p
 where prompt' :: FilePath -> String
       prompt' p = if null (splitPath p) then ""
                   else let dirs = map (\(c:_) -> [c]) (init $ splitPath p)
                        in joinPath (dirs ++ [last (splitPath p)])

-- | Run the MoniteM monad, with a given environment
run :: MoniteM a -> Env -> IO a
run m env = do
  res <- runExceptT $ evalStateT (runMonite m) env
  case res of
    Left err -> fail $ "error"
    Right a  -> return a

-- | Set the history file, and the custom completion function
mySettings :: IO (Settings IO)
mySettings = do
  home <- getHomeDirectory
  return $ setComplete myComplete $
    defaultSettings { historyFile = Just $ home ++ "/.monitehistory" }

-- | Read the end-user preferences from file
myPrefs :: IO Prefs
myPrefs = do
  home <- getHomeDirectory
  readPrefs (home ++ "/.moniterc") -- TODO: Document config file : 2015-03-11 - 10:14:25 (John)

-- | Check if the input is an exit code
isExitCode :: String -> Bool
isExitCode s = s `elem` ["quit", "exit", ":q"]

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
isBinaryCommand cmd = l == 1 && (not b1) && (not b2)
  where rcmd = reverse cmd
        b1   = last rcmd == ' '
        b2   = head rcmd == '.'
        l    = length (words rcmd)

-------------------------------------------------------------------------------
--
-- TODO: Extend haskeline with this completion function : 2015-03-11 - 11:25:27 (John)
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
