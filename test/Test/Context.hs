module Test.Context where

import Test.Tasty
import Test.Utils

spec :: TestEff TestTree
spec =
  testThese
    "Context Processing"
    [ testThis "Active context update" testUpdateActiveContext
    ]

testUpdateActiveContext :: TestEff ()
testUpdateActiveContext = do
