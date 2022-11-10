module Main where

import Test.Tasty (TestTree)

import Test.Compaction
import Test.Utils (TestEff)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  spec <- traverse (\comp -> runTestEff comp)
  defaultMain . testGroup "JSON-LD Tests"

specs :: [TestEff TestTree]
specs =
  [ Compaction.spec
  ]
