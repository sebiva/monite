module Main (main) where

import Monite.Interpret
import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Monad.IO.Class

main :: IO ()
main = do
  putStrLn "Hello, this failed"
  (fp, h) <- openTempFile "." ".temp"
  p <- createProcess $ process "./test/good/good.sh" (UseHandle h)
  h1 <- openFile fp ReadMode
  output <- hGetContents h1 -- TODO: Does not seem to read the file : 2015-03-03 - 11:12:37 (John)
  putStrLn output
  eOutput <- readFile $ "./test/good/good.sh.output"
  putStrLn eOutput
  rmoveFile fp
  if output == eOutput then return ()
  else exitFailure

process :: FilePath -> StdStream -> CreateProcess
process t output = CreateProcess { cmdspec = RawCommand "dist/build/monite/monite" [t],
                          cwd     = Nothing,
                          env     = Nothing,
                          std_in  = Inherit,
                          std_out = output,
                          std_err = Inherit,
                          close_fds = False,
                          create_group = False,
                          delegate_ctlc = False}


