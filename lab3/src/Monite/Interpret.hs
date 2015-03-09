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
import System.Directory ( getHomeDirectory, removeFile, setCurrentDirectory, doesDirectoryExist )
import System.IO
import System.IO.Error
import System.FilePath

import Control.Monad ( Monad, liftM, foldM )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT, MonadError (..) )
import Control.Monad.State.Lazy ( MonadState, StateT (..), evalStateT, get, modify )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Data.List (intersperse, intercalate)
import qualified Data.Map as M

-- | MoniteM monad which handles state, exceptions, and IO
newtype MoniteM a = Monite { runMonite :: StateT Env
                                            (ExceptT MoniteErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Env, MonadError MoniteErr)

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
  {-io $ putStrLn (show (myLexer s)) -- TODO : Debug-}
  case pProgram $ myLexer s of
    Ok tree -> do
      {-io $ putStrLn (show tree)    -- TODO : Debug-}
      {-io $ putStrLn "--------------------------"-}
      {-io $ putStrLn (printTree tree)-}
      {-io $ putStrLn "--------------------------"-}

      (eval tree)
      get :: MoniteM Env

    Bad err -> do
      io $ putStrLn $ "Unrecognized command: " ++ s
      get :: MoniteM Env


-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = do
  env <- get                        -- TODO: Debug : 2015-03-02 - 20:03:45 (John)
  {-io $ putStrLn $ show env      -- TODO: Debug : 2015-03-02 - 20:03:54 (John)-}
  mapM_ (\e -> evalExp' e Inherit Inherit) exps

-- | Evaluate an expression, catching and printing any error that occurs.
evalExp' :: Exp -> StdStream -> StdStream -> MoniteM ()
evalExp' e i o = catchError eval handle
  where eval       = evalExp e i o --runStateT evalExp e i o
        handle err = do
                      io $ putStrLn $ "Error executing: " ++ errCmd err
                      io $ putStrLn $ "In: " ++ errPath err
                      io $ putStrLn $ errMsg err


-- | Evaluate an expression -- TODO: Implement, compr, let, list : 2015-03-02 - 17:51:40 (John)
evalExp :: Exp -> StdStream -> StdStream -> MoniteM ()
evalExp e input output = case e of
  (ECompL e1 (Id i) []) -> eraseVar (Var i) >> return () -- erase var from env when done
  (ECompL e1 (Id i) ((LExp e2):es)) -> do
    extendEvalExp (Var i) e2 input             -- extend the environment with eval of e2
    evalExp e1 input output              -- eval e1 in new environment
    evalExp (ECompL e1 (Id i) es) input output -- keep evaluating the rest of the list comp expressions
  (EComp e1 (Id i) e2) -> do
    evalExpToStr e2 input $ \res -> do
      {-io $ putStrLn (show res)-}
      mapM_ (\s -> updateVar (Var i) [s] >> evalExp e1 input output) res
      eraseVar (Var i)
  (ELet (Id i) e) -> do
    extendEvalExp (Var i) e input              -- extend the environment with eval of e
  (ELetIn (Id i) e1 e2) -> do
    extendEvalExp (Var i) e1 input             -- extend the env with v := eval e1
    evalExp e2 input output              -- eval e2 in the updated environment
    eraseVar (Var i)                     -- erase var from env
  (EList ((LExp e):es)) -> undefined -- TODO: Not sure how meaningful this is : 2015-03-07 - 12:49:51 (John)
  (ECmd c) -> evalCmd c input output >> return ()

-- | Evaluate the expression with the stdout redirected to a temporary file, and
-- extend the environment with the value of the evaluated expression assigned to
-- the variable.
extendEvalExp :: Var -> Exp -> StdStream -> MoniteM ()
extendEvalExp v e input = do
  evalExpToStr e input $ \res -> do
    {-io $ putStrLn (show res)-}
    updateVar v res               -- update env with the var v, and the result of e1
    return ()

-- | Evaluate an expression, and then perform the given action f with the
-- output of the expression as a list of strings as input. Uses a temporary
-- file to store the output.
evalExpToStr :: Exp -> StdStream -> ([String] -> MoniteM a) -> MoniteM a
evalExpToStr e input f = do
  (fp, h) <- newTempFile
  evalExp e input (UseHandle h) -- Evaluate the expression with its output redirected to the temp file
  h <- reopenClosedFile fp      -- Since createProcess seems to close the file, we need to open it again.
  ss <- io $ hGetContents h
  ret <- f (lines ss)
  io $ removeFile fp            -- Clean up the temporary file
  return ret

-- | Evaluate the given command, using the provided pipes for I/O. Returns the
-- resulting pipes (may be redirected).
evalCmd :: Cmd -> StdStream -> StdStream -> MoniteM (StdStream, StdStream) -- String for testing
evalCmd c input output = case c of
  (CText (b:ts))        -> do
    {-(b:ts) <- foldM replaceVar [] (b:ts) -- TODO: Return a list of arguments-}
    case b of
     (TId (Id "cd")) -> do
        changeWorkingDirectory ts
        {-io $ putStrLn (show ts)-}
        return (input, output)
     _              -> do
        {-io $ putStrLn $ "CMD: " ++ show c-}
        (s:ss) <- readText (b:ts)
        env       <- get
        pHandle <- io $ tryIOError $ createProcess (proc' s ss (path env) input output)
        (i, o, _, p) <- case pHandle of
          Left e  -> throwError $ (err env (s:ss))
          Right h -> return h
        io $ waitForProcess p
        return (input, output)
  (CPipe c1 c2)     -> do
    (fp, h) <- newTempFile
    evalCmd c1 input (UseHandle h)
    h <- reopenClosedFile fp
    (i, o) <- evalCmd c2 (UseHandle h) output
    io $ removeFile fp -- Clean up the temporary file
    return (i, o)
  (COut c' t)      -> do
    f <- getFilename t
    h <- openFile' f WriteMode
    (i, o) <- evalCmd c' input (UseHandle h)
    io $ hClose h
    return (i, o)
  (CIn c' t)       -> do
    f <- getFilename t
    {-io $ putStrLn f-}
    h <- openFile' f ReadMode
    (i, o) <- evalCmd c' (UseHandle h) output
    io $ hClose h
    return (i, o)
  where err env c = Err {
      errPath = path env
    , errCmd  = "Process execution"
    , errMsg  = "Error invalid command: " ++ (intercalate " " c)
  }

replaceVar :: Text -> MoniteM [Text]
replaceVar = undefined

replace :: String -> String -> String -> String
replace "" _ _ = ""
replace s  m r
  | take (length m) s == m = r ++ replace (drop (length m) s) m r

parseVars :: Text -> [String]
parseVars = undefined

-- | Opens a file in the specified mode, throwing an error if something goes
-- wrong.
openFile' :: FilePath -> IOMode -> MoniteM Handle
openFile' f m = do
  env <- get
  mhandle <- io $ tryIOError (openFile f m)
  case mhandle of
    Left e  -> throwError (err env e)
    Right h -> return h
  where err env e = Err {
            errPath = path env
          , errCmd  = "File operation"
          , errMsg  = "Error opening file: " ++ f ++ " : " ++ msg e }
        msg e
          | isAlreadyInUseError e = "The file is already open"
          | isDoesNotExistError e = "The file does not exist"
          | isPermissionError e   = "Permission denied"

-- | Return a filename from a Text
getFilename :: Text -> MoniteM FilePath
getFilename t =
  case t of
    (TLit (Lit l)) -> return l
    (TVar (Var v)) -> do
      ss <- lookupVar (Var (tail v))
      return $ concat ss

-- | Change the working directory of the shell to the head of the provided list
-- of arguments. If the list is empty, the home directory is used.
changeWorkingDirectory :: [Text] -> MoniteM ()
changeWorkingDirectory ts = do
  t <- case ts of
        []    -> liftM (TLit . Lit . (++ "/")) (io getHomeDirectory)
        (t:_) -> return t
  [newPath] <- getText t
  env <- get
  finalPath <- io $ buildPath (path env) newPath
  {-let finalPath = modifyPath (path env) newPath -- TODO: Check that the path exists-}
  exists <- io (doesDirectoryExist finalPath)

  command <- readText ts
  let err = Err {
    errPath = path env
  , errCmd = "cd " ++ concat command
  , errMsg = "Can't cd to: " ++ finalPath ++ " : it does not exist"
  }
  if not exists then throwError err else return ()
  modify (\env -> env { path = finalPath } )
  io $ setCurrentDirectory finalPath
  return ()

-- | Builds a valid path from the old path and the new path
buildPath :: FilePath -> FilePath -> IO FilePath
buildPath old new = case new of
  ('~':'/':path)    -> do               -- if ~ , find the home directory
    home <- liftM addTrailingPathSeparator getHomeDirectory
    return $ home ++ (dropTrailingPathSeparator path)
  _             -> return $ fixPath (splitDirectories (old </> new)) []  -- fix path

-- | Given a split path, it will build a new path from the parsed path atoms,
-- going up on "..".
fixPath :: [FilePath] -> FilePath -> FilePath
fixPath [] path    = path
fixPath (s:ss) path
  | s == "."  = fixPath ss path
  | s == ".." = fixPath ss (takeDirectory path)
  | otherwise = fixPath ss (path </> s)

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
  {-io $ putStrLn (show env) -- TODO: Debug : 2015-03-02 - 21:14:08 (John)-}
  return ()

-- | Erase a var from the environment
eraseVar :: Var -> MoniteM ()
eraseVar v = do
  modify (\env -> env { vars = M.delete v (vars env) } )

-- | Lookup a var in the environment
lookupVar :: Var -> MoniteM [String]
lookupVar v = do
  env <- get
  case M.lookup v (vars env) of
    Nothing -> do
      env <- get
      throwError $ Err {
          errPath = path env
        , errCmd  = show v -- TODO: Should we keep the command in the state to be able to print it?
        , errMsg  = "Undefined variable: " ++ show v}
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
emptyEnv = do home <- getHomeDirectory -- TODO: Config instead? : 2015-03-02 - 22:33:21 (John) Maybe should start in the current working dir?
              return $ Env M.empty (home ++ "/") -- TODO: Not nice? : 2015-03-03 - 19:58:37 (John) - Either adding '/' everywhere or keeping without them everywhere. This would require special cases for "/" though, for example when performing "cd .."

-- | Convert a list of text to a list of string
readText :: [Text] -> MoniteM [String]
readText ts = do ss <- mapM getText ts       -- mapM (Text -> MoniteM [String]) -> [Text] -> MoniteM [String]
                 {-io $ putStrLn (show ss)-}
                 return $ concat ss

-- | Get the text as a list of strings from a literal. If it is a variable,
-- the value is looked up in the environment.
getText :: Text -> MoniteM [String]
getText (TLit (Lit s))  = return [s]   -- :: Text -> MoniteM [String]
getText (TVar (Var v))  = lookupVar (Var (tail v))  -- :: Text -> MoniteM [String]
getText (TId (Id i))    = return [i]

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io = liftIO
