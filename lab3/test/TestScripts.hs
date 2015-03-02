module Main (main) where

import Monite.Interpret

main :: IO ()
main = do
  s <- readFile "Test.sh"
  interpret s
  {-case interpret s of-}
    {-Bad _ -> exitFailure-}
