{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.IteratedSubset
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Generate fractal trails by the \"iterated subset\" construction,
-- iteratively replacing each segment with a given pattern.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.IteratedSubset
       (
         -- * Iterated subset algorithm

         iterTrail, refineSegment

         -- * Examples

         -- ** Example seed trails
         -- $seeds

       , koch
       , levy
       , zag
       , sqUp
       , sqUpDown
       , sqUpDown'

         -- ** Other stuff
         -- $other

       , snowflake

       , sqUpDownOverlay

       , IterTrailConfig(..), randITC, drawITC, drawITCScaled
       , randIterGrid
       ) where

-- Diagrams.Core.Points needed for V (Point a) instance on GHC < 7.6
import           Diagrams.Core.Points       ()
import           Diagrams.Prelude

import           Control.Monad              (replicateM)
import           Control.Monad.Random       (evalRandIO, getRandom, getRandomR)
import           Control.Monad.Random.Class (MonadRandom)
import           Data.Colour.SRGB           (sRGB)
import           Data.List.Split            (chunksOf)
import           Data.Maybe                 (mapMaybe)

------------------------------------------------------------
-- Iterated subset algorithm
------------------------------------------------------------

-- | Given a \"seed pattern\", produce a list of successive
--   refinements, where the nth trail in the list has iteratively had
--   all segments replaced by the seed pattern n times, starting from
--   a horizontal line.  In other words, the zeroth trail in the
--   output list is a horizontal unit segment, and each subsequent
--   trail is equal to the previous with all segments replaced by the
--   seed pattern.
iterTrail :: Trail R2 -> [Trail R2]
iterTrail t = iterate (mconcat . mapMaybe (refineSegment t) . trailSegments)
                      (fromOffsets [unitX])

-- | Use a trail to \"refine\" a segment, returning a scaled and/or
--   rotated copy of the trail with the same endpoint as the segment.
refineSegment :: Trail R2 -> Segment R2 -> Maybe (Trail R2)
refineSegment t seg
  | tOff == 0 || sOff == 0 = Nothing
  | otherwise              = Just $ t # scale k # rotateBy r
  where
    sOff = segOffset seg
    tOff = trailOffset t
    k    = magnitude sOff / magnitude tOff
    r    = direction sOff - direction tOff

------------------------------------------------------------
-- Examples
------------------------------------------------------------

--------------------------------------------------
-- Example seed trails

-- $seeds
-- These are just a few sample seed trails which give interesting
-- results, but by no means the only ones!  To view them, do something like
--
-- > (iterTrail koch !! 5) # strokeT
--
-- or use them as inspiration for creating your own seed trails.

-- | Seed for the Koch curve (side of the famous Koch 'snowflake').
koch :: (PathLike p, V p ~ R2) => p
koch = fromOffsets [unitX, unitX # rotateBy (1/6), unitX # rotateBy (-1/6), unitX]

-- | Seed for the Lévy dragon curve.
levy :: (PathLike p, V p ~ R2) => p
levy = fromOffsets [unitY, unitX]

-- | Strange zig-zag seed that produces a dense fractal path with lots
--   of triangles.
zag :: (PathLike p, V p ~ R2) => p
zag = fromOffsets [unitX, (-0.5) & 1, unitX]

-- | A \"square impulse\" seed which produces a quadratic von Koch
--   curve.
sqUp :: (PathLike p, V p ~ R2) => p
sqUp = fromOffsets [unitX, unitY, unitX, unit_Y, unitX]

-- | A \"double square impulse\" seed which produces fantastic
--   rectilinear spiral patterns.
sqUpDown :: (PathLike p, V p ~ R2) => p
sqUpDown = fromOffsets [unitX, unitY, unitX, 2 *^ unit_Y, unitX, unitY, unitX]

-- | Like 'sqUpDown' but with 'cubicSpline' applied to produce a curvy
--   version.
sqUpDown' :: (PathLike p, V p ~ R2) => p
sqUpDown' = cubicSpline False sqUpDown

--------------------------------------------------
-- Miscellaneous examples

-- $other
-- A random collection of other fun things you can do with
-- 'iterTrail'.  There is no particular emphasis on making these
-- configurable or generic; the point is just to suggest some fun
-- things you can do.  If you want to play with them, copy the source
-- code and modify it as you see fit.

-- | The famous Koch snowflake, made by putting three Koch curves
--   together. @snowflake n@ yields an order-@n@ snowflake.
snowflake :: Int -> Trail R2
snowflake n = iterateN 3 (rotateBy (-1/3)) edge
            # mconcat
            # close
  where edge = iterTrail koch !! n

-- | A cool diagram featuring successive iterations of @sqUpDown'@
--   superimposed atop one another.
sqUpDownOverlay :: Renderable (Path R2) b => Diagram b R2
sqUpDownOverlay
  = sized (Width 4)
  . mconcat
  . zipWith lc (iterate (blend 0.1 white) blue)
  . map strokeT
  . take 5
  . iterTrail
  $ sqUpDown'

--------------------------------------------------
-- Generating random iterated subset fractals

-- | Parameters to generate an iterated subset fractal.
data IterTrailConfig = ITC { seed  :: Trail R2       -- ^ The seed trail
                           , color :: Colour Double  -- ^ The line color to use
                           , iters :: Int            -- ^ Number of iterations
                           }

-- | Generate a random 'IterTrailConfig'.  This features many
--   hard-coded values.  If you want to play with it just copy the
--   code and modify it to suit.
randITC :: (MonadRandom m, Applicative m) => m IterTrailConfig
randITC = do
  -- use between two and five segments for the seed pattern
  numSegs <- getRandomR (2,5)

  -- should we make the seed pattern a cubic spline?
  spline  <- getRandom

  -- generate a random list of linear segments drawn from (-1,1)^2.
  s       <- fromOffsets <$>
                replicateM numSegs ((&) <$> getRandomR (-1,1) <*> getRandomR (-1,1))

  -- generate a random color.
  c       <- sRGB <$> getRandom <*> getRandom <*> getRandom

  -- generate a random number of iterations, with a lower bound of 3
  -- (since fewer is not very interesting) and an upper bound chosen
  -- to ensure we won't get more than 10000 segments in the final
  -- path.
  i       <- getRandomR (3, floor (logBase (fromIntegral numSegs :: Double) 10000))
  let s'
        | spline    = cubicSpline False s
        | otherwise = fromVertices s
  return $ ITC s' c i

-- | Generate an iterated subset fractal based on the given parameters.
drawITC :: Renderable (Path R2) b => IterTrailConfig -> Diagram b R2
drawITC (ITC s c i) = (iterTrail s !! i) # strokeT # lc c

-- | Like 'drawITC', but also scales, centers, and pads the result so
-- that it fits nicely inside a 4x4 box.
drawITCScaled
  :: (Renderable (Path R2) b, Backend b R2)
  => IterTrailConfig -> Diagram b R2
drawITCScaled itc
  = drawITC itc
  # sized (Dims 4 4)
  # centerXY
  # pad 1.1

-- | Create a grid of 100 random iterated subset fractals.  Impress
--   your friends!
randIterGrid :: (Renderable (Path R2) b, Backend b R2) => IO (Diagram b R2)
randIterGrid = do
  itcs <- evalRandIO (replicateM 100 randITC)
  return (vcat . map hcat . chunksOf 10 . map drawITCScaled $ itcs)
