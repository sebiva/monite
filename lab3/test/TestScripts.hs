module Main (main) where

import Monite.Interpret

main :: IO ()
main = do
  s <- readFile "test/Test.sh" -- TODO: Dynamic file placement? : 2015-03-02 - 14:43:00 (John)
  interpret s                  -- Interpret the test file
