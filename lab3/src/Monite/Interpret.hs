{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monite.Interpret (
    interpret     -- :: String -> IO ()
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs

import System.Process

import Control.Monad ( Monad )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.State.Lazy ( MonadState, StateT, evalStateT, get, modify )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Data.List (intersperse)
import qualified Data.Map as M

-- | MoniteM monad which handles state, exceptions, and IO
newtype MoniteM a = Monite { runMonite :: StateT Env
                                            (ExceptT MoniteErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Env)

-- | Monite shell environment, keep track of path and variables
data Env = Env {
  vars :: M.Map Var [String],
  path :: FilePath
} deriving (Show)

-- | Monite error
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
      putStrLn "--------------------------"

      res <- runExceptT $ evalStateT (runMonite (eval tree)) emptyEnv
      case res of
        Left err -> putStrLn "error"
        Right a  -> return ()

    Bad err -> do
      putStrLn err

-- | An empty environment
emptyEnv :: Env
emptyEnv = Env M.empty "." -- -- TODO: $HOME? / Conf : 2015-03-02 - 17:28:51 (John)

-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = do
  env <- get                        -- TODO: Debug : 2015-03-02 - 20:03:45 (John)
  liftIO $ putStrLn $ show env      -- TODO: Debug : 2015-03-02 - 20:03:54 (John)
  mapM_ evalExp exps

-- | Evaluate an expression -- TODO: Implement, compr, let, list : 2015-03-02 - 17:51:40 (John)
evalExp :: Exp -> MoniteM [String]
evalExp e = case e of
  (EComp e1 v e2)   -> undefined
  (ELet v e1 e2)    -> do
    vse1 <- evalExp e1             -- eval e1, and save string value of e1 -- TODO: This should not be output, but only saved in the variable : 2015-03-02 - 21:29:21 (John)
    updateVar v vse1               -- update env with the var v, and value of e1
    evalExp e2                     -- eval e2 in the updated environment
  (EList els)       -> undefined
  (ECmd c)          -> do
    (s:ss)    <- evalCmd c
    env       <- get
    (_,_,_,p) <- liftIO $ createProcess (proc' s ss (path env)) -- TODO: We need to create processes in another way : 2015-03-02 - 21:33:16 (John)
    liftIO $ waitForProcess p
    return []

-- | Evaluate the used command -- TODO: Implement pipe, out, in : 2015-03-02 - 17:51:18 (John)
evalCmd :: Cmd -> MoniteM ([String]) -- String for testing
evalCmd c = case c of
  (CText ts)        -> concatText ts
  (CPipe c1 c2)     -> undefined
  (COut c1 c2)      -> undefined
  (CIn c1 c2)       -> undefined

-- | Update a var in the environment
updateVar :: Var -> [String] -> MoniteM ()
updateVar v s = do
  modify (\env -> env { vars = M.insert v s (vars env) })
  env <- get
  liftIO $ putStrLn (show env) -- TODO: Debug : 2015-03-02 - 21:14:08 (John)
  return ()

-- | Lookup a var in the environment
lookupVar :: Var -> MoniteM [String]
lookupVar v = do
  env <- get
  case M.lookup v (vars env) of
    Nothing -> undefined -- TODO: Define : 2015-03-02 - 20:52:25 (John)
    Just v  -> return v

proc' :: FilePath -> [String] -> FilePath -> CreateProcess
proc' cmd args cwd = CreateProcess { cmdspec = RawCommand cmd args,
                                     cwd     = return $ cwd,
                                     env     = Nothing,
                                     std_in  = Inherit,
                                     std_out = Inherit,
                                     std_err = Inherit,
                                     close_fds = False,
                                     create_group = False,
                                     delegate_ctlc = False}


-- | Convert a list of text to a list of string
concatText :: [Text] -> MoniteM [String]
concatText ts = do ss <- mapM get ts       -- mapM (Text -> MoniteM [String]) -> [Text] -> MoniteM [String]
                   liftIO $ putStrLn (show ss)
                   return $ concat ss
  where get :: Text -> MoniteM [String]
        get (TLit (Lit s))  = return [s]   -- :: Text -> MoniteM [String]
        get (TVar v)        = lookupVar v  -- :: Text -> MoniteM [String]
