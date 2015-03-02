module Monite.Interpret (
    interpret     -- :: String -> IO ()
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs

import System.Process

import Data.List (intersperse)

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
eval (PProg exps) = mapM_ evalExp exps

evalExp :: Exp -> IO ()
evalExp e = case e of
  (EComp e1 v e2)   -> undefined
  (ELet v e1 e2)    -> undefined
  (EList els)       -> undefined
  (ECmd c)          -> do
    (s:ss) <- evalCmd c
    (_,_,_,p) <- createProcess (proc s ss)
    waitForProcess p
    return ()


evalCmd :: Cmd -> IO ([String]) -- String for testing
evalCmd c = case c of
  (CText ts)        -> return $ concatText ts
  (CPipe c1 c2)     -> undefined
  (COut c1 c2)      -> undefined
  (CIn c1 c2)       -> undefined

concatText :: [Text] -> [String]
concatText ts = map get ts
  where get (TLit (Lit s)) = s
        get (TVar (Var s)) = s
{-data ListEl =-}
   {-LExp Exp-}
  {-deriving (Eq,Ord,Show,Read)-}

{-data Cmd =-}
   {-CText [Text]-}
 {-| CPipe Cmd Cmd-}
 {-| COut Cmd Cmd-}
 {-| CIn Cmd Cmd-}
  {-deriving (Eq,Ord,Show,Read)-}

{-data Text =-}
   {-TLit Lit-}
 {-| TVar Var-}
  {-deriving (Eq,Ord,Show,Read)-}

