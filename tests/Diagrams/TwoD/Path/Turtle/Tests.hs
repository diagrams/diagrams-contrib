{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diagrams.TwoD.Path.Turtle.Tests
  ( tests
  ) where

import           Control.Arrow                        ((***))

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           Diagrams.Prelude
import           Diagrams.TwoD.Path.Turtle.Internal

tests :: [Test]
tests =
  [ testProperty "Moves forward correctly" movesForward
  , testProperty "Moves backward correctly" movesBackward
  , testProperty "Moves backward and forward correctly" movesBackwardAndForward
  , testProperty "Moves left correctly" movesLeft
  , testProperty "Moves right correctly" movesRight
  , testProperty "Current trail is empty when pen is up" trailEmptyWhenPenUp
  , testProperty "penHop creates a new path when pen is down" verifyPenHopWhenPenDown
  , testProperty "penHop does not create new path when pen is up" verifyPenHopWhenPenUp
  , testProperty "closeCurrent works correctly when no trail has started and pen is down" verifyCloseCurrent
  , testProperty "closeCurrent only produces a single new path" closeCurrentSingle
  ]

lineSegments :: Line V2 Double -> [Segment V2 Double]
lineSegments = toListOf segments

-- | The turtle moves forward by the right distance
movesForward :: TurtleState Double -> Property
movesForward t =  isPenDown t ==>
     diffPos      == round x  -- position is set correctly
  && lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # forward x
  diffPos :: Int
  diffPos      = round $ norm $ penPos t' .-. penPos t
  lenCurrTrail :: Int
  lenCurrTrail = round $ arcLength 0.0001 . last . lineSegments . unLoc . currTrail $ t'

-- | The turtle moves forward by the right distance
movesBackward :: TurtleState Double -> Property
movesBackward t = isPenDown t ==>
     diffPos      == round x  -- position is set correctly
  && lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # backward x
  diffPos :: Int
  diffPos      = round $ norm $ penPos t' .-. penPos t
  lenCurrTrail :: Int
  lenCurrTrail = round $ arcLength 0.0001 . last . lineSegments . unLoc . currTrail $ t'

-- | The turtle moves forward and backward by the same distance and returns to
-- the same position
movesBackwardAndForward :: TurtleState Double -> Property
movesBackwardAndForward t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
  && totalSegmentsAdded == 2
 where
  x                          = 2.0
  t'                         = t # forward x # backward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'
  totalSegmentsAdded         = (uncurry (-)) . (getTrailLength *** getTrailLength) $ (t',t)
  getTrailLength             = (length . lineSegments . unLoc . currTrail)

-- | The turtle moves left four times and returns to the same position
movesLeft  :: TurtleState Double -> Property
movesLeft t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  t'                         = t # forward x # left 90
                                 # forward x # left 90
                                 # forward x # left 90
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | The turtle moves right four times and returns to the same position
movesRight  :: TurtleState Double -> Property
movesRight t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  t'                         = t # forward x # right 90
                                 # forward x # right 90
                                 # forward x # right 90
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | When the trail is empty, @currTrail@ always remains empty and no new paths
-- are added
trailEmptyWhenPenUp :: TurtleState Double -> Property
trailEmptyWhenPenUp t = isPenDown t ==> currEmpty t'
 where
  t'           = t # penUp # forward 4 # backward 3

-- | Verify that the turtle adds a trail to @paths@ when pen is down
-- and @penHop@ is called.
verifyPenHopWhenPenDown :: TurtleState Double -> Property
verifyPenHopWhenPenDown t = isPenDown t ==> numPaths t' - numPaths t == 1
 where
  t' = t # forward 2.0 # penHop
  numPaths = length . paths

-- | Verify that the turtle does not add a trail to @paths@ when pen is up
-- and @penHop@ is called.
verifyPenHopWhenPenUp :: TurtleState Double -> Property
verifyPenHopWhenPenUp t = (not (isPenDown t) && currEmpty t) ==> numPaths t' == numPaths t
 where
  t' = t # forward 2.0 # penHop
  numPaths = length . paths

-- | Verify that calling @closeCurrent@ updates the turtle position to the beginning to the trail
verifyCloseCurrent :: TurtleState Double -> Property
verifyCloseCurrent t = (isPenDown t && currEmpty t) ==> penPos t' == origin
 where
  t' = t # setPenPos origin # forward 2.0 # right 90 # forward 3.0 # closeCurrent

-- | Verify that closeCurrent only produces a single new path (not two; see #13).
closeCurrentSingle :: TurtleState Double -> Property
closeCurrentSingle t = (isPenDown t) ==> ((length . paths $ t') == (succ . length . paths $ t))
  where
  t' = t # closeCurrent

currEmpty :: TurtleState Double -> Bool
currEmpty = null . lineSegments . unLoc . currTrail

-- There is no Show instance for Measure than fulfills `show . read :=
-- id`.  We define orphan instances here so that they do not polute
-- the library, but failing tests can show the geometry that caused
-- the failure.

instance Show (Measure n) where
  show _ = "<measure>"

deriving instance Show (PenStyle n)
deriving instance Show n => Show (TurtlePath n)
deriving instance Show n => Show (TurtleState n)

instance (Num n, Arbitrary n) => Arbitrary (Measure n) where
  arbitrary = oneof
              [ local <$> arbitrary
              , global <$> arbitrary
              , normalized <$> arbitrary]

-- | Arbitrary instance for the TurtleState type.
--
-- FIXME this arbitrary instance can generate
-- invalid turtle. For e.g. when pen is up, and
-- the current trail is not empty.
--
-- Currently we filter these out in the tests
instance Arbitrary (TurtleState Double) where
   arbitrary =
     TurtleState
       <$> arbitrary
       <*> arbitrary
       <*> ((@@deg) <$> arbitrary)
       <*> arbitrary
       <*> arbitrary
       <*> arbitrary

-- | Arbitrary instance for Diagrams type P2
instance Arbitrary (P2 Double) where
  arbitrary = p2 <$> arbitrary

-- | Arbitrary instance for TurtlePath
instance Arbitrary (TurtlePath Double) where
  arbitrary = TurtlePath <$> arbitrary <*> arbitrary

-- | Arbitrary instance of PenStyle
--
-- The color of the pen is chosen from black, blue or brown only
instance Arbitrary (PenStyle Double) where
  arbitrary = do
    penWidth_ <- arbitrary
    colorCode <- choose (1,3) :: Gen Int
    case colorCode of
      1 -> return $ PenStyle penWidth_  black
      2 -> return $ PenStyle penWidth_  blue
      3 -> return $ PenStyle penWidth_  brown
      _ -> error "Should not get here"

-- | Arbitrary instance of Segment
--
-- Currently this only generates linear segments only
instance Arbitrary (Segment V2 Double)  where
  arbitrary = do
    h <- (@@deg) <$> arbitrary
    x <- r2 <$> arbitrary
    return $ rotate h (straight x)

instance Arbitrary (Line V2 Double) where
  arbitrary = lineFromSegments <$> arbitrary

instance (Arbitrary a, Arbitrary (Point (V a) (N a))) => Arbitrary (Located a) where
  arbitrary = at <$> arbitrary <*> arbitrary

instance Arbitrary (Trail V2 Double) where
  arbitrary = wrapLine <$> arbitrary
