module Interpret where

import System.Environment

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs (Exp, Lit, Cmd)

-- | The main loop which interprets the shell commands executed by the user
interpret :: String -> IO ()
interpret file = do
  s <- readFile file
  putStrLn (show (myLexer s))
  case pProgram $ myLexer s of
    Ok tree -> do
      putStrLn (show tree)
      putStrLn "--------------------------"
      putStrLn (printTree tree)
      return ()
    Bad err -> do
      putStrLn err



