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
import System.Directory ( getHomeDirectory, removeFile )
import System.IO

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
  {-putStrLn (show (myLexer s)) -- TODO : Debug-}
  case pProgram $ myLexer s of
    Ok tree -> do
      putStrLn (show tree)    -- TODO : Debug
      putStrLn "--------------------------"
      putStrLn (printTree tree)
      putStrLn "--------------------------"

      env <- emptyEnv
      res <- runExceptT $ evalStateT (runMonite (eval tree)) env
      case res of
        Left err -> putStrLn "error"
        Right a  -> return ()

    Bad err -> do
      putStrLn err

-- | An empty environment
emptyEnv :: IO Env
emptyEnv = do home <- getHomeDirectory -- TODO: Config instead? : 2015-03-02 - 22:33:21 (John)
              return $ Env M.empty home

-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = do
  env <- get                        -- TODO: Debug : 2015-03-02 - 20:03:45 (John)
  liftIO $ putStrLn $ show env      -- TODO: Debug : 2015-03-02 - 20:03:54 (John)
  mapM_ (\e -> evalExp e Inherit Inherit) exps

-- | Evaluate an expression -- TODO: Implement, compr, let, list : 2015-03-02 - 17:51:40 (John)
evalExp :: Exp -> StdStream -> StdStream -> MoniteM ()
evalExp e input output = case e of
  (EComp e1 v e2)   -> undefined
  (ELet v e1 e2)    -> do
    (fp, h) <- newTempFile
    evalExp e1 input (UseHandle h)     -- Evaluate the expression with its output redirected to the temp file
    h <- reopenClosedFile fp -- Since createProcess seems to close the file, we need to open it again.
    vse1 <- liftIO $ hGetContents h
    liftIO $ removeFile fp             -- Clean up the temporary file
    liftIO $ putStrLn (show vse1)
    updateVar v (lines vse1)           -- update env with the var v, and the result of e1
    evalExp e2 input output            -- eval e2 in the updated environment
  (EList els)       -> undefined
  (ECmd c)          -> evalCmd c input output >> return ()

-- | Evaluate the used command -- TODO: Implement pipe, out, in : 2015-03-02 - 17:51:18 (John)
evalCmd :: Cmd -> StdStream -> StdStream -> MoniteM (StdStream, StdStream) -- String for testing
evalCmd c input output = case c of
  (CText ts)        -> do
    liftIO $ putStrLn $ "CMD: " ++ show c
    (s:ss) <- readText ts
    env       <- get
    (i,o,_,p) <- liftIO $ createProcess (proc' s ss (path env) input output)
    liftIO $ waitForProcess p
    return (input, output)
  (CPipe c1 c2)     -> do
    (fp, h) <- newTempFile
    evalCmd c1 input (UseHandle h)
    h <- reopenClosedFile fp
    (i, o) <- evalCmd c2 (UseHandle h) output
    liftIO $ removeFile fp -- Clean up the temporary file
    return (i, o)
  (COut c1 c2)      -> undefined
  (CIn c1 c2)       -> undefined

-- | Create a new temporary file
newTempFile :: MoniteM (FilePath, Handle)
newTempFile = liftIO $ openTempFile "." ".tmp.t"

-- | Re open a closed file in read mode
reopenClosedFile :: FilePath -> MoniteM (Handle)
reopenClosedFile fp = liftIO $ openFile fp ReadMode

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

-- | Run a command cmd with args in cwd with the provided input and output pipes
proc' :: FilePath -> [String] -> FilePath -> StdStream -> StdStream -> CreateProcess
proc' cmd args cwd input output =
  CreateProcess { cmdspec = RawCommand cmd args,
                  cwd     = return $ cwd,
                  env     = Nothing,
                  std_in  = input,
                  std_out = output,
                  std_err = Inherit,
                  close_fds = False,
                  create_group = False,
                  delegate_ctlc = False}


-- | Convert a list of text to a list of string
readText :: [Text] -> MoniteM [String]
readText ts = do ss <- mapM get ts       -- mapM (Text -> MoniteM [String]) -> [Text] -> MoniteM [String]
                 liftIO $ putStrLn (show ss)
                 return $ concat ss
  where get :: Text -> MoniteM [String]
        get (TLit (Lit s))  = return [s]   -- :: Text -> MoniteM [String]
        get (TVar v)        = lookupVar v  -- :: Text -> MoniteM [String]
