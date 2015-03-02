module Monite (main) where

import System.Environment (getArgs)
import System.Console.Haskeline
import Interpret (interpret)

-- | Starting the shell main loop with a possible script file as argument.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> interpret file
    _      -> getInput

-- | Get input from the user
getInput :: IO ()
getInput = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ "Input was: " ++ input
                         loop
