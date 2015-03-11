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
import Data.Maybe (fromJust)

import System.Console.Haskeline
import Control.Exception (AsyncException(..))

import System.Environment ( getEnvironment )

-- | MoniteM monad which handles state, exceptions, and IO
newtype MoniteM a = Monite { runMonite :: StateT Env
                                            (ExceptT MoniteErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Env, MonadError MoniteErr)

-- | Monite shell environment, keep track of path and variables
data Env = Env {
  vars :: M.Map Var [String],
  path:: FilePath
} deriving (Show)

type Var = String

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
      {-io $ putStrLn (show tree)   --  TODO : Debug-}
      {-io $ putStrLn "--------------------------"-}
      {-io $ putStrLn (printTree tree)-}
      {-io $ putStrLn "--------------------------"-}

      (eval tree)
      get :: MoniteM Env

    Bad err -> do
      io $ putStrLn $ "Unrecognized command: " ++ s
      get :: MoniteM Env


-- | Evaluate the abstract syntax tree, as parsed by the lexer, catching errors
-- thrown when executing it
eval :: Program -> MoniteM ()
eval (PProg exps) =
  catchError (mapM_ (\e -> evalExp e stdin stdout) exps) handle
  where handle err = do
                      io $ putStrLn $ "Error executing: " ++ errCmd err
                      io $ putStrLn $ "In: " ++ errPath err
                      io $ putStrLn $ errMsg err

-- | Evaluate an expression with its input and output from/to the provided
-- file handles
evalExp :: Exp -> Handle -> Handle -> MoniteM ()
evalExp e input output = case e of
  (ECompL e1 (Lit i) []) -> eraseVar i   -- erase var from env when done
  (ECompL e1 (Lit i) ((LExp (Lit s)):ss)) -> do
    -- Treat the literals as pure strings
    updateVar i [s]                      -- extend the environment with eval of e2
    evalExp e1 input output              -- eval e1 in new environment
    evalExp (ECompL e1 (Lit i) ss) input output -- keep evaluating the rest of the list comp.
  (EComp e1 (Lit i) e2) -> do
    evalExpToStr e2 input $ \res -> do   -- evalute the list-expression to strings
      -- Evaluate the expression for each of the list elements
      mapM_ (\s -> updateVar i [s] >> evalExp e1 input output) res
      eraseVar i --TODO: Write back the original value?
  (ELet (Lit i) e) -> do
    extendEvalExp i e input              -- extend the environment with eval of e
  (ELetIn (Lit i) e1 e2) -> do
    extendEvalExp i e1 input             -- extend the env with v := eval e1
    evalExp e2 input output              -- eval e2 in the updated environment
    eraseVar i                           -- erase var from env
  (EList []) -> io $ hFlush output
  (EList ((LExp (Lit l)):es)) -> do
    -- Treat list elements as pure strings
    io $ hPutStrLn output l
    evalExp (EList es) input output
  (ECmd c) -> evalCmd c input output
  (EStr s) -> io $ hPutStrLn output s >> hFlush output

-- | Evaluate the expression with the stdout redirected to a temporary pipe, and
-- extend the environment with the value of the evaluated expression assigned to
-- the variable.
extendEvalExp :: Var -> Exp -> Handle -> MoniteM ()
extendEvalExp v e input = do
  evalExpToStr e input $ updateVar v

-- | Evaluate an expression, and then perform the given action f with the
-- output of the expression as a list of strings as input.
evalExpToStr :: Exp -> Handle -> ([String] -> MoniteM a) -> MoniteM a
evalExpToStr e input f = do
  (i, o) <- io createPipe
  evalExp e input o -- Evaluate the expression with its output redirected to the pipe
  io $ hClose o -- Close the output end of the pipe to read from it
  ss <- io $ hGetContents i -- It seems hGetContents closes the pipe when everything is read -- TODO?
  f (lines ss)

-- | Evaluate the given command, using the provided pipes for I/O. Returns the
-- resulting pipes (may be redirected).
evalCmd :: Cmd -> Handle -> Handle -> MoniteM ()
evalCmd c input output = case c of
  (CText (b:ts)) -> do
                      ss <- replaceVarss (b:ts) -- TODO: Will this always have length at least one? (b:ts) will, since there is a nonempty in the grammar, but will ss?
                      {-io $ putStrLn $ "Res: " ++ show (ss) -- TODO: Debug-}
                      case b of
                        (Lit "cd") -> changeWorkingDirectory ts
                        _          -> runCmd ss input output
  (CPipe c1 c2)  -> do
                      (i, o) <- io createPipe
                      evalCmd c1 input o
                      evalCmd c2 i output
                      closePipe (i, o)
  (COut c' t)    -> do
                      f <- getFilename t
                      h <- openFile' f WriteMode
                      evalCmd c' input h
                      io $ hClose h
  (CIn c' t)     -> do
                      f <- getFilename t
                      h <- openFile' f ReadMode
                      evalCmd c' h output
                      io $ hClose h

-- | Runs the command as a process, with the first element as the binary and
-- the rest as arguments.
runCmd :: [String] -> Handle -> Handle -> MoniteM ()
{-runCmd []     input output = return ()-} --TODO: Should it be here? (See above)
runCmd c@(s:ss) input output = do
  env <- get
  let run = createProcess (proc' s ss (path env) (UseHandle input) (UseHandle output))
  -- Catch any errors that occur while running the process, and throw them as
  -- Monite errors instead so they can be cought and printed properly.
  eErrTup <- io $ tryIOError $ run
  (_, _, _, p) <- case eErrTup of
    Left e  -> throwError $ (err env)
    Right h -> return h

  -- Abort the command if Ctrl-C is pressed while it is being executed.
  -- Must use the InputT monad in order to use withInterrupt.
  merr <- io $ runInputT defaultSettings $ handl p $ withInterrupt $
    liftIO $ waitForProcess p >> return Nothing

  case merr of
    Nothing -> return ()
    Just m  -> throwError ((err env) {errMsg = m})
  where handl p r = flip handleInterrupt r $ do
                    -- Terminate the process and wait for it to exit to avoid zombies
                    liftIO $ terminateProcess p
                    liftIO $ waitForProcess p
                    return $ Just $ "Command aborted: " ++ (intercalate " " c)
        err env   = Err { errPath = path env
                        , errCmd  = "Process execution"
                        , errMsg  = "Invalid command: " ++ (intercalate " " c)
                        }

-- | Close both ends of a pipe
closePipe :: (Handle, Handle) -> MoniteM ()
closePipe (i, o) = io $ hClose i >> hClose o

-- | Replace all variables in a list of literals with their values
replaceVarss :: [Lit] -> MoniteM [String]
replaceVarss ls = mapM replaceVars ls >>= return . concat

-- | Replace all variables in a literal with their values
replaceVars :: Lit -> MoniteM [String]
replaceVars (Lit i) = liftM words (parseVars i)

-- | Replace all $var in a string with their definitions in the environment -- TODO: Only handles one var!!
parseVars :: String -> MoniteM String
parseVars s = do
  if var == "" then return s else do
    ss <- lookupVar var
    {-io $ putStrLn $ "Lookup: " ++ show ss-} -- TODO: Debug
    {-io $ putStrLn ("Var found in : " ++ before ++ intercalate " " ss ++ after)-} -- TODO: Debug
    rest <- parseVars after
    return (before ++ intercalate " " ss ++ rest)
  where before  = takeWhile (/='$') s
        var     = takeWhile valid (drop (length before + 1) s)
        after   = drop (length before + 1 + length var) s
        valid x = x `elem` ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

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

-- | Return a filename from a Literal, replacing any variables with their
-- values. The filename must not contain any spaces.
getFilename :: Lit -> MoniteM FilePath
getFilename l = do
  env <- get
  ss <- replaceVars l
  if length ss == 1 then return $ head ss
    else throwError $ Err {
        errPath = path env
      , errCmd  = "Getting filename"
      , errMsg  = "Filename must not contain spaces" }

-- | Change the working directory of the shell to the head of the provided list
-- of arguments. If the list is empty, the home directory is used.
changeWorkingDirectory :: [Lit] -> MoniteM ()
changeWorkingDirectory ts = do
  env <- get
  cmd <- replaceVarss ts
  newPath <- case cmd of
          []    -> liftM (++ "/") (io getHomeDirectory)
          (t:[]) -> return t
          ss     -> throwError $ err cmd env "To many arguments to cd"

  finalPath <- io $ buildPath (path env) newPath
  exists <- io (doesDirectoryExist finalPath)

  if not exists
    then throwError $
          err cmd env ("Can't cd to: " ++ finalPath ++ " : it does not exist")
     else return ()
  modify (\env -> env { path = finalPath } )
  io $ setCurrentDirectory finalPath
  return ()
  where err cmd env msg = Err {
            errPath = path env
            , errCmd = "cd " ++ concat cmd
            , errMsg = msg
            }

-- | Builds a valid path from the old path and the new path
buildPath :: FilePath -> FilePath -> IO FilePath
buildPath old new = case new of
  ('~':'/':path)    -> do               -- if ~ , find the home directory
    home <- liftM addTrailingPathSeparator getHomeDirectory
    return $ home ++ (dropTrailingPathSeparator path)
  _             -> return $ fixPath (splitDirectories (normalise (old </> new))) []  -- fix path

-- | Given a split path, it will build a new path from the parsed path atoms,
-- going up on "..".
fixPath :: [FilePath] -> FilePath -> FilePath
fixPath [] path    = path
fixPath (s:ss) path
  | s == "."  = fixPath ss path
  | s == ".." = fixPath ss (takeDirectory path)
  | otherwise = fixPath ss (path </> s)

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
              env <- liftM (map (\(k, v) -> (k, [v]))) getEnvironment
              return $ Env (M.fromList env) home

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io = liftIO
