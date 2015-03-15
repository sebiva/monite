{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monite.Interpret (
    MoniteM (..)
  , Env (..)
  , interpret     -- :: String -> IO ()
  , initEnv       -- :: IO Env
  , emptyEnv      -- :: FilePath -> Env
  )
where

import Grammar.ErrM (Err (..) )
import Grammar.Par (pProgram, myLexer)
import Grammar.Print (printTree)
import Grammar.Abs

import System.IO
import System.Process ( createPipe, createProcess, CreateProcess (..)
                      , CmdSpec (RawCommand),  StdStream (..) , waitForProcess
                      , terminateProcess)
import System.Directory ( getHomeDirectory, setCurrentDirectory
                        , doesDirectoryExist, getCurrentDirectory )
import System.IO.Error (tryIOError, isAlreadyInUseError
                                  , isDoesNotExistError
                                  , isPermissionError)
import System.Environment ( getEnvironment )
import System.FilePath
import System.Console.Haskeline

import Control.Monad ( Monad, liftM, foldM )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT, MonadError (..) )
import Control.Monad.State.Lazy ( MonadState, StateT (..), get, modify )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import qualified Data.Map as M
import Data.Maybe (fromJust)

-- | MoniteM monad which handles state, exceptions, and IO
newtype MoniteM a = Monite { runMonite :: ExceptT MoniteErr
                                           (StateT Env IO) a }
  deriving (  Functor, Applicative, Monad, MonadIO
            , MonadState Env, MonadError MoniteErr)

-- | Monite shell environment, keep track of path and variables
data Env = Env {
  vars :: [Context],
  cmd :: LExp,
  path :: FilePath,
  pipes :: (Handle,Handle)
} deriving (Show)

type Context = M.Map Var [String]

type Var = String

-- | Monite error type
data MoniteErr = Err {
  errPath :: FilePath,
  errCmd  :: String,
  errMsg  :: String
}

-- | Interpret a string, i.e., parsing it using the bnfc grammar and then
-- evaluate it as a program (the abstract syntax tree).
interpret :: String -> MoniteM Env
interpret s = do
  case pProgram $ myLexer s of
    Ok tree -> do
      eval tree
    Bad err -> do
      io $ putStrLn $ "Unrecognized command: " ++ s
  get

-- | Evaluate the abstract syntax tree, as parsed by the lexer, catching errors
-- thrown when executing it
eval :: Program -> MoniteM ()
eval (PProg exps) =
  catchError (mapM_ (\e -> addExp e >> evalLetExp e) exps) handle
  where handle err = do
                      io $ hPutStrLn stderr $ "Error executing: " ++ errCmd err
                      io $ hPutStrLn stderr $ "In: " ++ errPath err
                      io $ hPutStrLn stderr $ errMsg err
        addExp e   = modify (\env -> env {cmd = e})

-- | Evaluate a let-expression, updating the environment stack accordingly.
-- For 'let x = w', the variable x is set to w globaly. With 'let x = w1 in w2'
-- the variable x is only visible inside 'w2'. The righthandside must be Wrap.
evalLetExp :: LExp -> MoniteM ()
evalLetExp l = case l of
  (LLet (Lit v) (WPar ws))     -> do ss <- evalExpToStr (EWraps ws)
                                     updateVar v ss

  (LLet (Lit v) (WCmd c))      -> do ss <- replaceVarss c
                                     updateVar v ss
  (LLetIn (Lit v) (WPar ws) l) -> do ss <- evalExpToStr (EWraps ws)
                                     enterScope v ss
                                     evalLetExp l
                                     exitScope
  (LLetIn (Lit v) (WCmd c) l)  -> do ss <- replaceVarss c
                                     enterScope v ss
                                     evalLetExp l
                                     exitScope
  (LLe e)                      -> evalExp e

-- | Evaluate an expression - a list comprehension, list of commands or a list
-- of wraps.
evalExp :: Exp -> MoniteM ()
evalExp e = case e of
  (EComp lexp (Lit v) e) -> do
    ss <- evalExpToStr e
    mapM_ (\s -> enterScope v [s] >> evalLetExp lexp >> exitScope) ss
  (EList cs)             -> do
    sss <- mapM replaceVarss cs
    out <- liftM (snd . pipes) get
    mapM_ (io . (hPutStrLn out)) (map unwords sss)
  (EWraps ws)            -> do
    case ws of
      [WCmd c] -> evalCmd c
      (w:ws')  -> do
        ss <- evalWrapToStr False w          -- The binary shouldn't have quotes
        sss <- mapM (evalWrapToStr True) ws' -- The arguments should have quotes
        -- Interpret the resulting string as a command
        interpret (unwords (ss ++ concat sss)) >> return ()

-- | Evaluate a let expression into a list of strings, by running the
-- expression and splitting the lines in the output into a list.
evalLetExpToStr :: LExp -> MoniteM [String]
evalLetExpToStr e = do
  (i, o) <- io createPipe
  (inp, out) <- liftM pipes get
  setPipes (inp, o)
  evalLetExp e
  io $ hClose o             -- Close the write end of the pipe to read from it
  ss <- io $ hGetContents i
  setPipes (inp, out)
  return $ (lines ss)

-- | Evaluate an expression into a list of strings by converting it to a
-- top-level expression.
evalExpToStr :: Exp -> MoniteM [String]
evalExpToStr e = evalLetExpToStr (LLe e)

-- | Convert a Wrap into a list of strings by converting the commands it is
-- built of to Strings, and any contained '(( ))' will be reinterpreted.
evalWrapToStr :: Bool -> Wrap -> MoniteM [String]
evalWrapToStr quote w = case w of
  (WCmd c) -> replaceVarss c
  (WPar ws) -> liftM (addQuotes quote) (evalExpToStr (EWraps ws))
  where addQuotes True  ss = ["\"" ++ unwords ss ++ "\""]
        addQuotes _     ss = ss

-- | Evaluate the given command, using the current pipes for I/O and
-- redirections to file.
evalCmd :: Cmd -> MoniteM ()
evalCmd c = case c of
  (CText ts)     -> do
                      ss <- replaceVarss c
                      if null ss then return () else
                        case (head ss) of
                          "cd" -> changeWorkingDirectory (tail ss)
                          _    -> runCmd ss
  (CPipe c1 c2) -> do
                      (i, o) <- io createPipe
                      (inp, out) <- liftM pipes get
                      setPipes (inp, o)
                      evalCmd c1
                      setPipes (i, out)
                      evalCmd c2
                      closePipe (i, o)
                      setPipes (inp, out)
  (COut c' lit) -> do
                      f <- getFilename lit
                      h <- openFile' f WriteMode
                      (inp, out) <- liftM pipes get
                      setPipes (inp, h)
                      evalCmd c'
                      io $ hClose h
                      setPipes (inp, out)
  (CIn c' lit)  -> do
                      f <- getFilename lit
                      h <- openFile' f ReadMode
                      (inp, out) <- liftM pipes get
                      setPipes (h, out)
                      evalCmd c'
                      io $ hClose h
                      setPipes (inp, out)

-- | Replace all variables in a command with their values
replaceVarss :: Cmd -> MoniteM [String]
replaceVarss c = case c of
  (CText ts) -> mapM replaceVars (map textToStr ts) >>= return . concat
  (CPipe c1 c2) -> do
    ss1 <- replaceVarss c1
    ss2 <- replaceVarss c2
    return (ss1 ++ ["|"] ++ ss2)
  (COut c' (Lit o)) -> liftM (++[">"]++[o]) (replaceVarss c')
  (CIn c' (Lit i)) -> liftM (++["<"]++[i]) (replaceVarss c')
  where textToStr :: Text -> String
        textToStr t = case t of
                      (TStr s) -> s
                      (TLit (Lit s)) -> s


-- | Replace all variables in a literal with their values. Also replaces any
-- '~' by the value of $HOME.
replaceVars :: String -> MoniteM [String]
replaceVars s = do home <- liftM concat $ lookupVar "HOME"
                   liftM (words . replaceTilde home) (parseVars s)
  where replaceTilde _  ""       = ""
        replaceTilde home ('~':ss) = home ++ replaceTilde home ss
        replaceTilde home (c  :ss) = c:(replaceTilde home ss)

-- | Takes a command in the form of a nonempty list of strings. Runs the
-- command as a process, with the first element as the binary and the rest as
-- arguments.
runCmd :: [String] -> MoniteM ()
runCmd c@(s:ss) = do
  env <- get
  let (inp, out) = pipes env
      run = createProcess (proc' s ss (path env) (UseHandle inp) (UseHandle out))
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
                    return $ Just $ "Command aborted: " ++ unwords c
        err env   = Err { errPath = path env
                        , errCmd  = printTree (cmd env)
                        , errMsg  = "Invalid command: " ++ unwords c
                        }
-- | Set the current pipes
setPipes :: (Handle, Handle) -> MoniteM ()
setPipes p = modify (\env -> env {pipes = p})

-- | Close both ends of a pipe
closePipe :: (Handle, Handle) -> MoniteM ()
closePipe (i, o) = io $ hClose i >> hClose o

-- | Replace all $var in a string with their definitions in the environment
parseVars :: String -> MoniteM String
parseVars s = do
  if var == "" then return s else do
    ss <- lookupVar var
    rest <- parseVars after
    return (before ++ unwords ss ++ rest)
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
          , errCmd  = printTree (cmd env)
          , errMsg  = "Error opening file: " ++ f ++ " : " ++ msg e }
        msg e
          | isAlreadyInUseError e = "The file is already open"
          | isDoesNotExistError e = "The file does not exist"
          | isPermissionError e   = "Permission denied"

-- | Return a filename from a Literal, replacing any variables with their
-- values. The filename must not contain any spaces.
getFilename :: Lit -> MoniteM FilePath
getFilename (Lit l) = do
  env <- get
  ss <- replaceVars l
  if length ss == 1 then return $ head ss
    else throwError $ Err {
        errPath = path env
      , errCmd  = printTree (cmd env)
      , errMsg  = "Filename must not contain spaces" }

-- | Change the working directory of the shell to the head of the provided list
-- of arguments. If the list is empty, the home directory is used.
changeWorkingDirectory :: [String] -> MoniteM ()
changeWorkingDirectory command = do
  env <- get
  newPath <- case command of
          []    -> liftM (++ "/") (io getHomeDirectory)
          (t:[]) -> return t
          ss     -> throwError $ err env "Too many arguments to cd"

  let finalPath = buildPath (path env) newPath
  exists <- io (doesDirectoryExist finalPath)

  if not exists
    then throwError $
          err env ("Can't cd to: " ++ finalPath ++ " : it does not exist")
     else return ()
  modify (\env -> env { path = finalPath } )
  io $ setCurrentDirectory finalPath
  return ()
  where err env msg = Err {
            errPath = path env
            , errCmd = printTree (cmd env)
            , errMsg = msg
            }

-- | Builds a valid path from the old path and the new path
buildPath :: FilePath -> FilePath -> FilePath
buildPath old new = fixPath (splitDirectories (normalise (old </> new))) []

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
  env <- get
  updateVar' v s (vars env) []

-- | Update a variable in the most local context it occurs in. Takes a list of
-- contexts, and a list of contexts already looked through to create the
-- resulting environment.
updateVar' :: Var -> [String] -> [Context] -> [Context] -> MoniteM ()
updateVar' v ss (c:[]) rest = modify (\env ->
                                env {vars = rest ++ [M.insert v ss c] })
updateVar' v ss (c:cs) rest = do
  case M.lookup v c of
    Nothing -> updateVar' v ss cs (rest ++ [c])
    Just _  -> modify (\env -> env {vars = rest ++ ((M.insert v ss c) : cs) })

-- | Create a new scope with the supplied variable
enterScope :: Var -> [String] -> MoniteM ()
enterScope v s = modify (\env -> env {vars = M.insert v s M.empty : (vars env)})

-- | Exit the current scope
exitScope :: MoniteM ()
exitScope = modify (\env -> env {vars = tail (vars env)})

-- | Lookup a variable in the environment stack, returning a list contining the
-- empty string if the variable does not exist.
lookupVar :: Var -> MoniteM [String]
lookupVar v = do
  vs <- liftM vars get
  case lookupVar' v vs of
    -- The value of an undefined variable is the empty string
    Nothing -> return [""]
    Just v  -> return v

-- | Lookup a variable in the environment stack, returning the topmost one.
lookupVar' :: Var -> [Context] -> Maybe [String]
lookupVar' v []     = Nothing
lookupVar' v (c:cs) = case M.lookup v c of
  Just ss -> return ss
  Nothing -> lookupVar' v cs

-- | Run a command cmd with args in cwd with the provided input and output pipes
proc' :: FilePath     -- ^ The binary to run
         -> [String]  -- ^ Arguments to the binary
         -> FilePath  -- ^ The current working directory
         -> StdStream -- ^ The input pipe
         -> StdStream -- ^ The output pipe
            -> CreateProcess
proc' cmd args cwd inp out =
  CreateProcess { cmdspec = RawCommand cmd args,
                  cwd     = return $ cwd,
                  env     = Nothing,
                  std_in  = inp,
                  std_out = out,
                  std_err = Inherit,
                  close_fds = False,
                  create_group = False,
                  delegate_ctlc = False}

-- | An initial environment containing the system variables and the current path
initEnv :: IO Env
initEnv = do home <- getCurrentDirectory
             env <- liftM (map (\(k, v) -> (k, [v]))) getEnvironment
             return $ (emptyEnv home) { vars = [M.fromList env]}

-- | An empty environment
emptyEnv :: FilePath -> Env
emptyEnv path = Env [M.empty] (lexp "") path (stdin, stdout)
  where lexp s = LLe $ EWraps $ [WCmd $ CText $ [TStr s]]

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io = liftIO
