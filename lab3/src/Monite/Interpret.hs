{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monite.Interpret (
    interpret     -- :: String -> IO ()
  , emptyEnv
  , MoniteM (..)
  , Env (..)
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs

import System.Process
import System.Directory ( getHomeDirectory, removeFile, setCurrentDirectory )
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
  path:: FilePath
} deriving (Show)

-- | Monite error
data MoniteErr = Err {
  errPath :: FilePath,
  errCmd  :: String,
  errMsg  :: String
}

-- | The main loop which interprets the shell commands executed by the user
interpret :: String -> MoniteM Env
interpret s = do
  io $ putStrLn (show (myLexer s)) -- TODO : Debug
  case pProgram $ myLexer s of
    Ok tree -> do
      io $ putStrLn (show tree)    -- TODO : Debug
      io $ putStrLn "--------------------------"
      io $ putStrLn (printTree tree)
      io $ putStrLn "--------------------------"

      (eval tree)
      get :: MoniteM Env

    Bad err -> do
      io $ putStrLn err
      get :: MoniteM Env


-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = do
  env <- get                        -- TODO: Debug : 2015-03-02 - 20:03:45 (John)
  io $ putStrLn $ show env      -- TODO: Debug : 2015-03-02 - 20:03:54 (John)
  mapM_ (\e -> evalExp e Inherit Inherit) exps

-- | Evaluate an expression -- TODO: Implement, compr, let, list : 2015-03-02 - 17:51:40 (John)
evalExp :: Exp -> StdStream -> StdStream -> MoniteM ()
evalExp e input output = case e of
  (EComp e1 v (EList ([]))) -> do return ()
  (EComp e1 v (EList ((LExp e2):es))) -> do
    extendEvalExp v e2 input
    evalExp e1 input output
    evalExp (EComp e1 v (EList es)) input output
  (ELet v e)        -> do
    extendEvalExp v e input
  (ELetIn v e1 e2)  -> do
    extendEvalExp v e1 input
    evalExp e2 input output            -- eval e2 in the updated environment
  (EList ((LExp e):es)) -> undefined
  (ECmd c)          -> evalCmd c input output >> return ()
  where
    extendEvalExp :: Var -> Exp -> StdStream -> MoniteM ()
    extendEvalExp v e input = do
      (fp, h) <- newTempFile
      evalExp e input (UseHandle h) -- Evaluate the expression with its output redirected to the temp file
      h <- reopenClosedFile fp      -- Since createProcess seems to close the file, we need to open it again.
      vse <- io $ hGetContents h
      io $ removeFile fp            -- Clean up the temporary file
      io $ putStrLn (show vse)
      updateVar v (lines vse)       -- update env with the var v, and the result of e1
      return ()

-- | Evaluate the used command
evalCmd :: Cmd -> StdStream -> StdStream -> MoniteM (StdStream, StdStream) -- String for testing
evalCmd c input output = case c of
  (CText (b:ts))        ->
    case b of
     (TLit (Lit ('c':'d':[]))) -> do -- TODO: Nicer way? : 2015-03-03 - 13:13:03 (John)
        changeWorkingDirectory ts
        io $ putStrLn (show ts)
        return (input, output)
     _              -> do
        io $ putStrLn $ "CMD: " ++ show c
        (s:ss) <- readText (b:ts)
        env       <- get
        (i,o,_,p) <- io $ createProcess (proc' s ss (path env) input output)
        io $ waitForProcess p
        return (input, output)
  (CPipe c1 c2)     -> do
    (fp, h) <- newTempFile
    evalCmd c1 input (UseHandle h)
    h <- reopenClosedFile fp
    (i, o) <- evalCmd c2 (UseHandle h) output
    io $ removeFile fp -- Clean up the temporary file
    return (i, o)
  (COut c' t)      -> do -- TODO: Catch errors when opening files
    f <- getFilename t
    h <- io $ openFile f WriteMode
    (i, o) <- evalCmd c' input (UseHandle h)
    io $ hClose h
    return (i, o)
  (CIn c' t)       -> do -- TODO: Catch errors when opening files
    f <- getFilename t
    io $ putStrLn f
    h <- io $ openFile f ReadMode
    (i, o) <- evalCmd c' (UseHandle h) output
    io $ hClose h
    return (i, o)

-- | Return a filename from a Text
getFilename :: Text -> MoniteM FilePath
getFilename t =
  case t of
    (TLit (Lit l)) -> return l
    (TVar v) -> do
      ss <- lookupVar v
      return $ concat ss

changeWorkingDirectory :: [Text] -> MoniteM ()
changeWorkingDirectory [t] = do
  [newPath] <- getText t
  env <- get
  let finalPath = createPath (path env) newPath
  modify (\env -> env { path = finalPath } )
  io $ setCurrentDirectory finalPath
  return ()
  where createPath old new = case new of
          ('/':ts) -> ('/':ts)
          ts       -> old ++ ts

-- | Create a new temporary file
newTempFile :: MoniteM (FilePath, Handle)
newTempFile = io $ openTempFile "." ".tmp.t"

-- | Re open a closed file in read mode
reopenClosedFile :: FilePath -> MoniteM (Handle)
reopenClosedFile fp = io $ openFile fp ReadMode

-- | Update a var in the environment
updateVar :: Var -> [String] -> MoniteM ()
updateVar v s = do
  modify (\env -> env { vars = M.insert v s (vars env) })
  env <- get
  io $ putStrLn (show env) -- TODO: Debug : 2015-03-02 - 21:14:08 (John)
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


-- | An empty environment
emptyEnv :: IO Env
emptyEnv = do home <- getHomeDirectory -- TODO: Config instead? : 2015-03-02 - 22:33:21 (John)
              return $ Env M.empty (home ++ ['/']) -- TODO: Not nice? : 2015-03-03 - 19:58:37 (John)

-- | Convert a list of text to a list of string
readText :: [Text] -> MoniteM [String]
readText ts = do ss <- mapM getText ts       -- mapM (Text -> MoniteM [String]) -> [Text] -> MoniteM [String]
                 io $ putStrLn (show ss)
                 return $ concat ss

getText :: Text -> MoniteM [String]
getText (TLit (Lit s))  = return [s]   -- :: Text -> MoniteM [String]
getText (TVar v)        = lookupVar v  -- :: Text -> MoniteM [String]

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io i = liftIO i
