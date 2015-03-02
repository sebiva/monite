{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monite.Interpret (
    interpret     -- :: String -> IO ()
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs

import System.Process (createProcess, waitForProcess, proc )

import Control.Monad ( Monad )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Data.List (intersperse)
import qualified Data.Map as M

newtype MoniteM a = Monite { runMonite :: StateT Env
                                            (ExceptT MoniteErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Env)

data Env = Env {
  vars :: M.Map Var String,
  path :: FilePath
}

data MoniteErr = Err {
  errPath :: FilePath,
  errCmd  :: String,
  errMsg  :: String
}


-- | The main loop which interprets the shell commands executed by the user
interpret :: String -> IO ()
interpret s = do
  putStrLn (show (myLexer s)) -- TODO : Debug
  case pProgram $ myLexer s of
    Ok tree -> do
      putStrLn (show tree)    -- TODO : Debug
      putStrLn "--------------------------"
      putStrLn (printTree tree)

      res <- runExceptT $ evalStateT (runMonite (eval tree)) emptyEnv
      case res of
        Left err -> putStrLn "error"
        Right a  -> return ()

    Bad err -> do
      putStrLn err

emptyEnv :: Env
emptyEnv = Env M.empty "." -- -- TODO: $HOME? / Conf : 2015-03-02 - 17:28:51 (John)

-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = mapM_ evalExp exps

evalExp :: Exp -> MoniteM ()
evalExp e = case e of
  (EComp e1 v e2)   -> undefined
  (ELet v e1 e2)    -> undefined
  (EList els)       -> undefined
  (ECmd c)          -> do
    (s:ss)    <- evalCmd c
    (_,_,_,p) <- liftIO $ createProcess (proc s ss)
    liftIO $ waitForProcess p
    return ()


evalCmd :: Cmd -> MoniteM ([String]) -- String for testing
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

