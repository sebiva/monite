module Main (main) where

import System.Environment (getArgs)
import System.Console.Haskeline
import System.Directory ( getHomeDirectory, removeFile )

import Monite.Interpret

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT, get, modify, lift )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import qualified Data.Map as M

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  env <- emptyEnv
  case args of
    [file] -> do s <- readFile file               -- read the script file
                 run (interpret s) env            -- interpret the script file
                 return ()
    _      -> do getInput env                     -- get user commands

-- | Get commands from the user
getInput :: Env -> IO ()
getInput env = runInputT defaultSettings (loop env)         -- input loop w/ default sets
  where
    loop :: Env -> InputT IO ()
    loop env = do
      minput <- getInputLine "% "                 -- get user command
      case minput of
        Nothing -> return ()                      -- nothing entered
        Just "quit" -> return ()                  -- quit the shell loop
        Just input -> do env <- liftIO $ run (interpret input) env -- interpret user command
                         loop env

run :: MoniteM a -> Env -> IO a
run m env = do
  res <- runExceptT $ evalStateT (runMonite m) env
  case res of
    Left err -> fail $ "error"
    Right a  -> return a
