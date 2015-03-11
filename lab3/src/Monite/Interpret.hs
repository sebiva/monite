{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monite.Interpret (
    interpret     -- :: String -> IO ()
  , initEnv
  , emptyEnv
  , MoniteM (..)
  , Env (..)
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

import Control.Exception (AsyncException(..))
import Control.Monad ( Monad, liftM, foldM )
import Control.Applicative ( Applicative )
import Control.Monad.Except ( ExceptT, runExceptT, MonadError (..) )
import Control.Monad.State.Lazy ( MonadState, StateT (..), get, modify )
import Control.Monad.IO.Class ( liftIO, MonadIO )

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust)



-- | MoniteM monad which handles state, exceptions, and IO
newtype MoniteM a = Monite { runMonite :: StateT Env
                                            (ExceptT MoniteErr IO) a }
  deriving (  Functor, Applicative, Monad, MonadIO
            , MonadState Env, MonadError MoniteErr)

-- | Monite shell environment, keep track of path and variables
data Env = Env {
  vars :: [Context],
  cmd :: LExp,
  path:: FilePath
} deriving (Show)

type Context = M.Map Var [String]

type Var = String

-- | Monite error
data MoniteErr = Err {
  errPath :: FilePath,
  errCmd  :: String,
  errMsg  :: String
}


-- | The main loop which interprets the shell commands executed by the user.
-- Returns the resulting environment.
interpret :: String -> MoniteM Env
interpret s = interpret' s stdin stdout

-- | Interpret a string using the provided pipes
interpret' :: String -> Handle -> Handle -> MoniteM Env
interpret' s inp out = do
  case pProgram $ myLexer s of
    Ok tree -> do
      eval tree inp out
    Bad err -> do
      io $ putStrLn $ "Unrecognized command: " ++ s
  get

-- | Evaluate the abstract syntax tree, as parsed by the lexer, catching errors
-- thrown when executing it
eval :: Program -> Handle -> Handle -> MoniteM ()
eval (PProg exps) inp out =
  catchError (mapM_ (\e -> addExp e >> evalLExp e inp out) exps) handle
  where handle err = do
                      io $ putStrLn $ "Error executing: " ++ errCmd err
                      io $ putStrLn $ "In: " ++ errPath err
                      io $ putStrLn $ errMsg err
        addExp e   = modify (\env -> env {cmd = e})

evalLExp :: LExp -> Handle -> Handle -> MoniteM ()
evalLExp l inp out = case l of
  (LLet (Lit v) w)     -> do res <- evalWrapToStr w inp
                             updateVar v res
  (LLetIn (Lit v) w l) -> do res <- evalWrapToStr w inp
                             enterScope v res
                             evalLExp l inp out
                             exitScope
  (LLe e)              -> evalExp e inp out


evalExp :: Exp -> Handle -> Handle -> MoniteM ()
evalExp e inp out = case e of
  (EComp lexp (Lit v) e) -> undefined
  (EList cs)             -> undefined
  (EWraps ws)            -> undefined

evalWrapper :: Wrap -> Handle -> Handle -> MoniteM ()
evalWrapper w inp out = case w of
  (WPar ws) -> undefined
  (WCmd c)  -> undefined

evalCmd :: Cmd -> Handle -> Handle -> MoniteM ()
evalCmd c inp out = case c of
  (CText ts)        -> undefined
  (CPipe c1 c2)     -> undefined
  (COut c' (Lit o)) -> undefined
  (CIn c' (Lit i))  -> undefined

evalWrapToStr :: Wrap -> Handle -> MoniteM [String]
evalWrapToStr w inp = case w of
  (WCmd c) -> replaceVarss c
  (WPar ws) -> do
    sss <- mapM (\w -> evalWrapToStr w inp) ws

    (i, o) <- io $ createPipe
    interpret' (unwords $ concat sss) inp o
    io $ hClose o
    liftM lines $ io $ hGetContents i
    {-(i, o) <- io createPipe-}
    {-evalExp e inp o-}
    {-io $ hClose o             -- Close the write end of the pipe to read from it-}
    {-ss <- io $ hGetContents i-}
    {-f (lines ss)-}

-- | Replace all variables in a list of literals with their values
replaceVarss :: Cmd -> MoniteM [String]
replaceVarss c = case c of
  (CText ts) -> mapM replaceVars (map textToStr ts) >>= return . concat
  (CPipe c1 c2) -> do
    ss1 <- replaceVarss c1
    ss2 <- replaceVarss c2
    return (ss1 ++ ["|"] ++ ss2)
  (COut c' (Lit o)) -> liftM (++[">"]++[o]) (replaceVarss c')
  (CIn c' (Lit i)) -> liftM (++["<"]++[i]) (replaceVarss c')

-- | Replace all variables in a literal with their values
replaceVars :: String -> MoniteM [String]
replaceVars s = liftM words (parseVars s)

textToStr :: Text -> String
textToStr t = case t of
  (TStr s) -> s
  (TLit (Lit s)) -> s


{--- | Evaluate an expression with its input and output from/to the provided-}
{--- file handles-}
{-evalExp :: Exp -> Handle -> Handle -> MoniteM ()-}
{-evalExp e inp out = case e of-}
  {-(ECompL e1 (Lit v) [])                  -> return ()-}
  {-(ECompL e1 (Lit v) ((LExp (Lit s)):ss)) -> do-}
    {--- Treat the literals as pure strings-}
    {-enterScope v [s]-}
    {-evalExp e1 inp out-}
    {-evalExp (ECompL e1 (Lit v) ss) inp out-}
    {-exitScope-}

  {-(EComp e1 (Lit v) e2)         -> do-}
    {-evalExpToStr e2 inp $ \res  -> do   -- evalute the list-expression to strings-}
      {--- Evaluate the expression for each of the list elements-}
      {-mapM_ (\s -> enterScope v [s] >> evalExp e1 inp out >> exitScope) res-}

  {-(ELet (Lit v) e)       ->-}
    {-evalExpToStr e inp $ \res -> do    -- extend the environment with eval of e-}
      {-updateVar v res-}
  {-(ELetIn (Lit v) e1 e2) ->-}
    {-evalExpToStr e1 inp $ \res -> do-}
      {-enterScope v res                 -- extend the env with v := eval e1-}
      {-evalExp e2 inp out               -- eval e2 in the updated environment-}
      {-exitScope-}

  {-(EList ls) -> do-}
    {--- Treat list elements as pure strings and just print them-}
    {-mapM (\(LExp (Lit l)) -> io $ hPutStrLn out l ) ls-}
    {-io $ hFlush out-}

  {-(ECmd c) -> evalCmd c inp out-}
  {-(EStr s) -> io $ hPutStrLn out s >> hFlush out-}

{--- | Evaluate an expression into a list of strings, and then perform the given -}
{--- action f with this list as input.-}
{-evalExpToStr :: Exp -> Handle -> ([String] -> MoniteM a) -> MoniteM a-}
{-evalExpToStr e inp f = do-}
  {-(i, o) <- io createPipe-}
  {-evalExp e inp o-}
  {-io $ hClose o             -- Close the write end of the pipe to read from it-}
  {-ss <- io $ hGetContents i-}
  {-f (lines ss)-}

{--- | Evaluate the given command, using the provided pipes for I/O. Returns the-}
{--- resulting pipes (may be redirected).-}
{-evalCmd :: Cmd -> Handle -> Handle -> MoniteM ()-}
{-evalCmd c inp out = case c of-}
  {-(CText (b:ts)) -> do-}
                      {-ss <- replaceVarss (b:ts)-}
                      {-[>io $ putStrLn $ "Res: " ++ show (ss) -- TODO: Debug<]-}
                      {-if null ss then return () else-}
                        {-case (head ss) of-}
                          {-"cd" -> changeWorkingDirectory (tail ss)-}
                          {-_    -> runCmd ss inp out-}
  {-(CPipe c1 c2)  -> do-}
                      {-(i, o) <- io createPipe-}
                      {-evalCmd c1 inp o-}
                      {-evalCmd c2 i out-}
                      {-closePipe (i, o)-}
  {-(COut c' t)    -> do-}
                      {-f <- getFilename t-}
                      {-h <- openFile' f WriteMode-}
                      {-evalCmd c' inp h-}
                      {-io $ hClose h-}
  {-(CIn c' t)     -> do-}
                      {-f <- getFilename t-}
                      {-h <- openFile' f ReadMode-}
                      {-evalCmd c' h out-}
                      {-io $ hClose h-}

{--- | Runs the command as a process, with the first element as the binary and-}
{--- the rest as arguments.-}
{-runCmd :: [String] -> Handle -> Handle -> MoniteM ()-}
{-[>runCmd []     inp out = return ()<] --TODO: Should it be here? (See above)-}
{-runCmd c@(s:ss) inp out = do-}
  {-env <- get-}
  {-let run = createProcess (proc' s ss (path env) (UseHandle inp) (UseHandle out))-}
  {--- Catch any errors that occur while running the process, and throw them as-}
  {--- Monite errors instead so they can be cought and printed properly.-}
  {-eErrTup <- io $ tryIOError $ run-}
  {-(_, _, _, p) <- case eErrTup of-}
    {-Left e  -> throwError $ (err env)-}
    {-Right h -> return h-}

  {--- Abort the command if Ctrl-C is pressed while it is being executed.-}
  {--- Must use the InputT monad in order to use withInterrupt.-}
  {-merr <- io $ runInputT defaultSettings $ handl p $ withInterrupt $-}
    {-liftIO $ waitForProcess p >> return Nothing-}

  {-case merr of-}
    {-Nothing -> return ()-}
    {-Just m  -> throwError ((err env) {errMsg = m})-}
  {-where handl p r = flip handleInterrupt r $ do-}
                    {--- Terminate the process and wait for it to exit to avoid zombies-}
                    {-liftIO $ terminateProcess p-}
                    {-liftIO $ waitForProcess p-}
                    {-return $ Just $ "Command aborted: " ++ (intercalate " " c)-}
        {-err env   = Err { errPath = path env-}
                        {-, errCmd  = printTree (cmd env)-}
                        {-, errMsg  = "Invalid command: " ++ (intercalate " " c)-}
                        {-}-}

-- | Close both ends of a pipe
closePipe :: (Handle, Handle) -> MoniteM ()
closePipe (i, o) = io $ hClose i >> hClose o
-- | Replace all $var in a string with their definitions in the environment
parseVars :: String -> MoniteM String
parseVars s = do
  if var == "" then return s else do
    ss <- lookupVar var
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

  finalPath <- io $ buildPath (path env) newPath
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
buildPath :: FilePath -> FilePath -> IO FilePath
buildPath old new = case new of
  ('~':'/':path) -> do               -- if ~ , find the home directory
    home <- liftM addTrailingPathSeparator getHomeDirectory
    return $ home ++ (dropTrailingPathSeparator path)
  _     -> return $ fixPath (splitDirectories (normalise (old </> new))) []

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

-- | Lookup a variable in the environment stack
lookupVar :: Var -> MoniteM [String]
lookupVar v = do
  vs <- liftM vars get
  case lookupVar' v vs of
    Nothing -> do
      env <- get
      throwError $ Err {
          errPath = path env
        , errCmd  = printTree (cmd env)
        , errMsg  = "Undefined variable: " ++ show v}
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
             return $ Env [M.fromList env] (lexp "") home
-- | TODO: Slask
lexp :: String -> LExp
lexp s = LLe $ EWraps $ [WCmd $ CText $ [TStr s]]

-- | An empty environment
emptyEnv :: FilePath -> Env
emptyEnv path = Env [M.empty] (lexp "") path

-- | Shorthand for io actions
io :: (MonadIO m) => IO a -> m a
io = liftIO
