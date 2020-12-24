{-# LANGUAGE ImplicitParams #-}

module Approx where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.CallStack (HasCallStack, SrcLoc, callStack)
import Test.HUnit.Base (Assertion)
import Test.HUnit.Lang
  ( FailureReason (..),
    HUnitFailure (..),
  )

type Epsilon = Float

class Show a => Approx a where
  isApproxEqual :: Epsilon -> a -> a -> Bool

instance Approx a => Approx [a] where
  isApproxEqual e v1 v2 =
    length v1 == length v2 && and (zipWith (isApproxEqual e) v1 v2)

instance (Approx a, Approx b) => Approx (Either a b) where
  isApproxEqual e v1 v2 = case (v1, v2) of
    (Left _, Right _) -> False
    (Right _, Left _) -> False
    (Left l1, Left l2) -> isApproxEqual e l1 l2
    (Right r1, Right r2) -> isApproxEqual e r1 r2

instance (Approx a, Approx b) => Approx (a, b) where
  isApproxEqual e (v1Left, v1Right) (v2Left, v2Right) =
    isApproxEqual e v1Left v2Left && isApproxEqual e v1Right v2Right

instance Approx Float where
  isApproxEqual e v1 v2 = e >= abs (v1 - v2)

isApproxEqual' :: Approx a => (c -> [a]) -> Epsilon -> c -> c -> Bool
isApproxEqual' toList' e v1 v2 = isApproxEqual e (toList' v1) (toList' v2)

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

shouldApproxBe ::
  (HasCallStack, Approx a, ?epsilon :: Epsilon) => a -> a -> Assertion
shouldApproxBe actual expected =
  unless (isApproxEqual ?epsilon actual expected) $
    throwIO
      ( HUnitFailure location $
          ExpectedButGot
            (Just $ "maximum margin of error: " ++ show ?epsilon)
            (show expected)
            (show actual)
      )
