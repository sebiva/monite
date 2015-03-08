module Main (main) where

import System.Environment (getArgs)
import System.Console.Haskeline
import System.Directory ( getHomeDirectory, removeFile, setCurrentDirectory, getCurrentDirectory )

import Monite.Interpret

import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT, get, modify, lift )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import qualified Data.Map as M

-- TODO: Fix file placement! : 2015-03-06 - 16:08:11 (John)
mySettings :: FilePath -> Settings IO
mySettings path = defaultSettings { historyFile = Just $ path ++ "/monitehistory" }

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  env <- emptyEnv
  case args of
    [file] -> do s <- readFile file               -- read the script file
                 path <- getCurrentDirectory
                 run (interpret s) (Env M.empty (path ++ ['/']))            -- interpret the script file
                 return ()
    _      -> do setCurrentDirectory (path env)
                 inputLoop env                     -- get user commands

-- | Get commands from the user
inputLoop :: Env -> IO ()
inputLoop env = do
  path <- getHomeDirectory
  runInputT (mySettings path) (loop env)         -- input loop w/ default sets
  where
    loop :: Env -> InputT IO ()
    loop env = do
      let prompt = path env -- TODO: Makeup? : 2015-03-03 - 16:52:39 (John)
      minput <- getInputLine (prompt ++ " λ: ")                 -- get user command
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
