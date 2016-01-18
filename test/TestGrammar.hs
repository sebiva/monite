module Main (main) where

import Monite.Test

main :: IO ()
main = do
  -- The test file directory, and the binary to test with
  -- NOTE: Make sure that the Test binary has been properly built in the
  -- Grammars dir
  let dirScripts  = "test/grammars/good/"
      grammarBin  = "./src/Grammar/Test"

  -- Run the tests
  runTests grammarBin dirScripts
