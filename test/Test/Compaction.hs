module Test.Compaction where

import Test.Tasty
import Test.Utils

spec :: TestEff TestTree
spec =
  testThese
    "Compaction"
    [ testThis "[#t0001] drop free-floating nodes" t0001
    ]

-- | #t0001: drop free-floating nodes
t0001 :: TestEff ()
t0001 = undefined
