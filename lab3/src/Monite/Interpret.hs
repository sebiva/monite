module Monite.Interpret (
    interpret     -- :: String -> IO ()
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs (Exp, Lit, Cmd, Program)

-- | The main loop which interprets the shell commands executed by the user
interpret :: String -> IO ()
interpret s = do
  putStrLn (show (myLexer s)) -- TODO : Debug
  case pProgram $ myLexer s of
    Ok tree -> do
      putStrLn (show tree)    -- TODO : Debug
      putStrLn "--------------------------"
      putStrLn (printTree tree)
      eval tree               -- Evaluate the tree
    Bad err -> do
      putStrLn err

-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> IO ()
eval p = return ()
