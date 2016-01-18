module Main (main) where

import Monite.Test

main :: IO ()
main = do
  -- The test file directory, and the binary to test with
  let dirScripts  = "test/scripts/good/"
      moniteBin   = "./dist/build/moniteshell/moniteshell"

  -- Run the tests
  runTests moniteBin dirScripts
