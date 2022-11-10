{-# LANGUAGE DataKinds #-}

module Test.Utils
  ( testThis
  , testThese

    -- * Assertion functions
  , assertBool
  , assertEqual
  , assertFailure
  , assertRight
  , assertRight'
  , assertLeft
  , assertLeft'

    -- * TestEff
  , TestEff
  , runTestEff
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Effectful
import Effectful.Fail (Fail, runFailIO)
import GHC.Stack (HasCallStack)
import Optics.Core
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

type TestEff = Eff '[Fail, IOE]

runTestEff :: TestEff a -> IO a
runTestEff comp =
  comp
    & runFailIO
    & runEff

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  let test = runTestEff assertion
  pure $ Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

{-| Make sure an expected value is the same as the actual one.
 Usage:
 >>> assertEqual expected actual
-}
assertEqual :: (Eq a, Show a) => a -> a -> TestEff ()
assertEqual expected actual = liftIO $ Test.assertEqual "" expected actual

assertFailure :: (MonadIO m) => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertRight :: HasCallStack => Either a b -> TestEff b
assertRight (Left _a) = liftIO $ Test.assertFailure "Test return Left instead of Right"
assertRight (Right b) = pure b

assertRight' :: Either a b -> TestEff ()
assertRight' = void . assertRight

assertLeft :: HasCallStack => Either a b -> TestEff a
assertLeft (Left a) = pure a
assertLeft (Right _b) = liftIO $ Test.assertFailure "Test return Right instead of Left"

assertLeft' :: Either a b -> TestEff ()
assertLeft' = void . assertLeft
