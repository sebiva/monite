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
import System.FilePath ( isPathSeparator )

import Control.Monad ( Monad, liftM )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT, MonadError (..) )
import Control.Monad.State.Lazy ( MonadState, StateT (..), evalStateT, get, modify )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Data.List (intersperse)
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
      io $ putStrLn $ "Unrecognized command: " ++ s
      get :: MoniteM Env


-- | Evaluate the abstract syntax tree, as parsed by the lexer
eval :: Program -> MoniteM ()
eval (PProg exps) = do
  env <- get                        -- TODO: Debug : 2015-03-02 - 20:03:45 (John)
  io $ putStrLn $ show env      -- TODO: Debug : 2015-03-02 - 20:03:54 (John)
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
  (ECompL e1 v []) -> eraseVar v >> return () -- erase var from env when done
  (ECompL e1 v ((LExp e2):es)) -> do
    extendEvalExp v e2 input             -- extend the environment with eval of e2
    evalExp e1 input output              -- eval e1 in new environment
    evalExp (ECompL e1 v es) input output -- keep evaluating the rest of the list comp expressions
  (EComp e1 v e2) -> do
    evalExpToStr e2 input $ \res -> do
      io $ putStrLn (show res)
      mapM_ (\s -> updateVar v [s] >> evalExp e1 input output) res
      eraseVar v
  (ELet v e) -> do
    extendEvalExp v e input              -- extend the environment with eval of e
  (ELetIn v e1 e2) -> do
    extendEvalExp v e1 input             -- extend the env with v := eval e1
    evalExp e2 input output              -- eval e2 in the updated environment
    eraseVar v                           -- erase var from env
  (EList ((LExp e):es)) -> undefined -- TODO: Not sure how meaningful this is : 2015-03-07 - 12:49:51 (John)
  (ECmd c) -> evalCmd c input output >> return ()

-- | Evaluate the expression with the stdout redirected to a temporary file, and
-- extend the environment with the value of the evaluated expression assigned to
-- the variable.
extendEvalExp :: Var -> Exp -> StdStream -> MoniteM ()
extendEvalExp v e input = do
  evalExpToStr e input $ \res -> do
    io $ putStrLn (show res)
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
  (CText (b:ts))        ->
    case b of
     (TLit (Lit "cd")) -> do
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
  (COut c' t)      -> do
    f <- getFilename t
    h <- openFile' f WriteMode
    (i, o) <- evalCmd c' input (UseHandle h)
    io $ hClose h
    return (i, o)
  (CIn c' t)       -> do
    f <- getFilename t
    io $ putStrLn f
    h <- openFile' f ReadMode
    (i, o) <- evalCmd c' (UseHandle h) output
    io $ hClose h
    return (i, o)

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
    (TVar v) -> do
      ss <- lookupVar v
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

-- | Modify the old path with the new. If new is a subdirectory, they are
-- simply concatenated. If new is "..", the result is up one from "old".
-- -- TODO: Probably not needed anymore : 2015-03-08 - 23:07:44 (John)
modifyPath :: FilePath -> FilePath -> FilePath
modifyPath old new = case new of
  ('.':'.':[])  -> upDir old
  ('.':[])      -> old
  ('/':ts)      -> ('/':ts)
  []            -> old
  ts            -> old ++ ts ++ if last ts /= '/' then "/" else ""

-- | Builds a valid path from the old path and the new path
buildPath :: FilePath -> FilePath -> IO FilePath
buildPath old new = case new of
  path@('/':ts) -> return $ path    -- if absolute, simply return the new path
  ('~':path)    -> do               -- if ~ , find the home directory
    home <- getHomeDirectory
    return $ home ++ path
  _             -> return $ fixPath (parsePath (old ++ new)) [] -- fix path

-- | Given a parsed path, it will build a new path from the parsed path atoms
fixPath :: [FilePath] -> FilePath -> FilePath
fixPath [] path     = path ++ "/"
fixPath (s:ss) path
  | s == ""   = fixPath ss path-- TODO: Correct? : 2015-03-08 - 22:46:30 (John)
  | s == "."  = fixPath ss path
  | s == ".." = fixPath ss (upDir path)
  | otherwise = fixPath ss (path ++ "/" ++ s)

-- | Simply parse a path into a list of the parts separated by the path
-- separator ('/')
--
-- E.g. "/path/to/some/dir/" -> ["path", "to", "some", "dir"]
--
parsePath :: FilePath -> [FilePath]
parsePath []   = []
parsePath path = (nxt path) : (parsePath (rst path))
  where nxt = takeWhile (not . isPathSeparator ) . dropWhile (isPathSeparator)
        rst = dropWhile (not . isPathSeparator) . dropWhile (isPathSeparator)

-- | Go up one step from a valid filepath ('/' at the end).
upDir :: FilePath -> FilePath
upDir [] = []
upDir x   = reverse . dropWhile (=='/') . dropWhile (/='/') . reverse . init $ x

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
                 io $ putStrLn (show ss)
                 return $ concat ss

-- | Get the text as a list of strings from a literal. If it is a variable,
-- the value is looked up in the environment.
getText :: Text -> MoniteM [String]
getText (TLit (Lit s))  = return [s]   -- :: Text -> MoniteM [String]
getText (TVar v)        = lookupVar v  -- :: Text -> MoniteM [String]

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io = liftIO
