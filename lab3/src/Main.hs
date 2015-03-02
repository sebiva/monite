module Main (main) where

import System.Environment (getArgs)
import System.Console.Haskeline
import Monite.Interpret (interpret)
import Control.Monad.IO.Class (liftIO)

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do s <- readFile file               -- read the script file
                 interpret s                      -- interpret the script file
    _      -> getInput                            -- get user commands

-- | Get commands from the user
getInput :: IO ()
getInput = runInputT defaultSettings loop         -- input loop w/ default sets
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "                 -- get user command
      case minput of
        Nothing -> return ()                      -- nothing entered
        Just "quit" -> return ()                  -- quit the shell loop
        Just input -> do liftIO $ interpret input -- interpret user command
                         loop
