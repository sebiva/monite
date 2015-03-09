module Main (main) where

import Monite.Interpret
import System.Process
import System.IO
import System.Exit
import System.Directory
import Control.Monad.IO.Class

main :: IO ()
main = do
  (_, _, _, p) <- createProcess $ parseGrammarFile
  waitForProcess p
  return ()

parseGrammarFile = CreateProcess {
  cmdspec = RawCommand "cat ./test/good/grammars.sh | ./src/Grammar/Test" [],
  cwd     = Nothing,
  env     = Nothing,
  std_in  = Inherit,
  std_out = Inherit,
  std_err = Inherit,
  close_fds = False,
  create_group = False,
  delegate_ctlc = False}
