module Monite.Test (
    runTests
  , runTest
  , process

) where

import Monite.Interpret
import System.Process
import System.IO
import System.Exit (exitFailure)
import System.Directory (setCurrentDirectory, getDirectoryContents, removeFile)
import Control.Monad.IO.Class
import Control.Monad (foldM)
import Data.List (isSuffixOf, sort)

-- | Run all tests using the supplied binary, and the directory to the test
-- files
runTests :: FilePath -> FilePath -> IO ()
runTests bin dirTests = do
  -- Get all files in the test directory
  files <- getDirectoryContents dirTests
  -- Filter out the scripts, and create their relative paths
  let scripts = map (dirTests ++) $ filter (".sh" `isSuffixOf`) files
  -- Test all the scripts
  passed <- foldM test True scripts
  if passed then return () else exitFailure
  where
    test :: Bool -> FilePath -> IO Bool
    test b path = do
      passed <- runTest bin path
      return $ b && passed

-- | Run a test scripts, and check if the output is the same as the expected
-- output
runTest :: FilePath -> FilePath -> IO Bool
runTest bin path = do
  (i, o) <- createPipe
  (_, _, _, p) <- createProcess $ process bin path (UseHandle o)
  waitForProcess p
  hClose o
  output <- hGetContents i
  eOutput <- readFile (path ++ ".output")
  if sort (lines output) == sort (lines eOutput) then return True
  else do putStrLn ("----------------------------------------------------")
          putStrLn ("-- Testfile: " ++ path ++ " failed")
          putStrLn ("----------------------------------------------------")
          putStrLn ("Output:\n" ++ output)
          putStrLn ("Expected:\n" ++ eOutput)
          return False

-- | Spawn a process with the moniteshell running a script file
process :: FilePath -> FilePath -> StdStream -> CreateProcess
process bin t output = CreateProcess { cmdspec = RawCommand bin [t],
                          cwd     = Nothing,
                          env     = Nothing,
                          std_in  = Inherit,
                          std_out = output,
                          std_err = Inherit,
                          close_fds = False,
                          create_group = False,
                          delegate_ctlc = False}
