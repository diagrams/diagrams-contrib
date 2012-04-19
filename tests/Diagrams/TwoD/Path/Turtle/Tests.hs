{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diagrams.TwoD.Path.Turtle.Tests
  ( tests
  ) where

import Control.Arrow ((***))

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle.Internal

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
  ]


-- | The turtle moves forward by the right distance
movesForward :: Turtle
             -> Property
movesForward t =  isPenDown t ==>
     diffPos      == round x  -- position is set correctly
  && lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # forward x
  diffPos :: Int
  diffPos      = round $ magnitude $ penPos t' .-. penPos t
  lenCurrTrail :: Int
  lenCurrTrail = round $ flip arcLength 0.0001 . head . trailSegments . snd . currTrail $ t'

-- | The turtle moves forward by the right distance
movesBackward :: Turtle
             -> Property
movesBackward t =  isPenDown t ==>
     diffPos      == round x  -- position is set correctly
  && lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # backward x
  diffPos :: Int
  diffPos      = round $ magnitude $ penPos t' .-. penPos t
  lenCurrTrail :: Int
  lenCurrTrail = round $ flip arcLength 0.0001 . head . trailSegments . snd . currTrail $ t'

-- | The turtle moves forward and backward by the same distance and returns to
-- the same position
movesBackwardAndForward :: Turtle
                        -> Property
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
  getTrailLength             = (length . trailSegments . snd . currTrail)

-- | The turtle moves left four times and returns to the same position
movesLeft  :: Turtle
           -> Property
movesLeft t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  turn                       = 90
  t'                         = t # forward x # left turn
                                 # forward x # left turn
                                 # forward x # left turn
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | The turtle moves right four times and returns to the same position
movesRight  :: Turtle
            -> Property
movesRight t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  turn                       = 90
  t'                         = t # forward x # right turn
                                 # forward x # right turn
                                 # forward x # right turn
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | When the trail is empty, @currTrail@ always remains empty and no new paths
-- are added
trailEmptyWhenPenUp :: Turtle
                    -> Property
trailEmptyWhenPenUp t = isPenDown t ==> trailIsEmpty
 where
  t'           = t # penUp # forward 4 # backward 3
  trailIsEmpty = null . trailSegments . snd . currTrail $ t'

-- | Verify that the turtle adds a trail to @paths@ when pen is down
-- and @penHop@ is called.
verifyPenHopWhenPenDown :: Turtle
                        -> Property
verifyPenHopWhenPenDown t = isPenDown t ==> (numPaths t') - (numPaths t) == 1
 where 
  t' = t # forward 2.0 # penHop
  numPaths = length . paths

-- | Verify that the turtle does not add a trail to @paths@ when pen is up
-- and @penHop@ is called.
verifyPenHopWhenPenUp :: Turtle
                      -> Property
verifyPenHopWhenPenUp t = not (isPenDown t) && (null . trailSegments . snd . currTrail $ t) ==>  (numPaths t') == (numPaths t)
 where 
  t' = t # forward 2.0 # penHop
  numPaths = length . paths

-- | Verify that calling @closeCurrent@ updates the turtle position to the beginning to the trail
verifyCloseCurrent :: Turtle
                   -> Property
verifyCloseCurrent t = (isPenDown t)  && (null . trailSegments . snd . currTrail $ t) ==> (penPos t') == origin
 where
  t' = t # setPenPos origin # forward 2.0 # right 90 # forward 3.0 # closeCurrent
-- | Arbitrary instance for the Turtle type.
--
-- FIXME this arbitrary instance can generate
-- invalid turtle. For e.g. when pen is up, and
-- the current trail is not empty.
--
-- Currently we filter these out in the tests
instance Arbitrary Turtle where
   arbitrary =
     Turtle <$> arbitrary
            <*> arbitrary
            <*> (Deg <$> arbitrary)
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

-- | Arbitrary instance for Diagrams type P2
instance Arbitrary P2 where
  arbitrary = p2 <$> arbitrary

-- | Arbitrary instance for TurtlePath
instance Arbitrary TurtlePath where
  arbitrary = TurtlePath <$> arbitrary <*> arbitrary

-- | Arbitrary instance of PenStyle
--
-- The color of the pen is chosen from black, blue or brown only
instance Arbitrary PenStyle where
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
instance Arbitrary (Segment R2)  where
  arbitrary = do
    h <- Deg <$> arbitrary
    x <- r2 <$> arbitrary
    return $ rotate h (Linear x)

instance Arbitrary (Trail R2) where
  arbitrary = Trail <$> arbitrary <*> (return False)
