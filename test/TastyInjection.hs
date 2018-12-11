module TastyInjection (assertT, assertEx, defaultMain, testGroup) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
import Control.Monad

assertT title expected result = testCase title (assertEqual "" expected result)

newtype EC = EC ErrorCall deriving (Show)

instance Eq EC where
  x == y = (show x) == (show y)

assertException :: EC -> IO a -> IO ()
assertException (EC ex) action =
  handleJust isWanted (const $ return ()) $ do
      action
      assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

assertEx title ex f = testCase title $ assertException (EC ex) $ evaluate f