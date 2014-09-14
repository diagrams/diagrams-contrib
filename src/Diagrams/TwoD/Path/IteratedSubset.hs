{-# LANGUAGE ExplicitForAll        #-}
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

import           Control.Lens               ((^.))
import           Control.Monad              (replicateM)
import           Control.Monad.Random       (evalRandIO, getRandom, getRandomR)
import           Control.Monad.Random.Class (MonadRandom)
import           Data.Colour.SRGB           (sRGB)
import           Data.List.Split            (chunksOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Typeable
import           System.Random              (Random)

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
--
--   > import Diagrams.TwoD.Path.IteratedSubset
--   > iterTrailEx = vcat' (with & sep .~ 0.3) . map strokeLine . take 5
--   >             $ iterTrail koch
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_iterTrailEx.svg#diagram=iterTrailEx&width=200>>
iterTrail :: (RealFloat n, Ord n) => Trail' Line V2 n -> [Trail' Line V2 n]
iterTrail t = iterate (mconcat . mapMaybe (refineSegment t) . lineSegments)
                      (fromOffsets [unitX])

-- | Use a trail to \"refine\" a segment, returning a scaled and/or
--   rotated copy of the trail with the same endpoint as the segment.
refineSegment :: RealFloat n =>
                 Trail' Line V2 n -> Segment Closed V2 n ->
                 Maybe (Trail' Line V2 n)
refineSegment t seg
  | tOff == zero || sOff == zero = Nothing
  | otherwise              = Just $ t # scale k # rotate r
  where
    sOff = segOffset seg
    tOff = lineOffset t
    k    = norm sOff / norm tOff
    r    = (sOff^._theta) ^-^ (tOff^._theta)

------------------------------------------------------------
-- Examples
------------------------------------------------------------

--------------------------------------------------
-- Example seed trails

-- $seeds

-- These are just a few sample seed trails which give interesting
-- results, but by no means the only ones!  Use them directly, or use
-- them as inspiration for creating your own seed trails.

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > showTrail n t = hcat' (with & sep .~ 0.2) [ iters !! 1, iters !! n ]
-- >             # centerXY # pad 1.1
-- >   where iters = map strokeLine $ iterTrail t

-- | Seed for the Koch curve (side of the famous Koch 'snowflake').
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_kochD.svg#diagram=kochD&width=400>>
koch :: (TrailLike t, Vn t ~ V2 n, Floating n) => t
koch = fromOffsets [unitX, unitX # rotateBy (1/6), unitX # rotateBy (-1/6), unitX]

-- > kochD = showTrail 4 koch

-- | Seed for the Lévy dragon curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_levyD.svg#diagram=levyD&width=400>>
levy :: (TrailLike t, Vn t ~ V2 n) => t
levy = fromOffsets [unitY, unitX]

-- > levyD = showTrail 9 levy

-- | Strange zig-zag seed that produces a dense fractal path with lots
--   of triangles.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_zagD.svg#diagram=zagD&width=400>>
zag :: (TrailLike t, Vn t ~ V2 n) => t
zag = fromOffsets [unitX, (-0.5) ^& 1, unitX]

-- > zagD = showTrail 5 zag

-- | A \"square impulse\" seed which produces a quadratic von Koch
--   curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpD.svg#diagram=sqUpD&width=400>>
sqUp :: (TrailLike t, Vn t ~ V2 n) => t
sqUp = fromOffsets [unitX, unitY, unitX, unit_Y, unitX]

-- > sqUpD = showTrail 3 sqUp

-- | A \"double square impulse\" seed which produces fantastic
--   rectilinear spiral patterns.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownD.svg#diagram=sqUpDownD&width=400>>
sqUpDown :: (TrailLike t, Vn t ~ V2 n) => t
sqUpDown = fromOffsets [unitX, unitY, unitX, 2 *^ unit_Y, unitX, unitY, unitX]

-- > sqUpDownD = showTrail 3 sqUpDown

-- | Like 'sqUpDown' but with 'cubicSpline' applied to produce a curvy
--   version.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownD2.svg#diagram=sqUpDownD2&width=400>>
sqUpDown' :: (TrailLike t, Vn t ~ V2 n) => t
sqUpDown' = cubicSpline False sqUpDown

-- > sqUpDownD2 = showTrail 3 sqUpDown'

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
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_snowflake4.svg#diagram=snowflake4&width=300>>
snowflake :: (RealFloat n, Ord n) => Int -> Trail V2 n
snowflake n = iterateN 3 (rotateBy (-1/3)) edge
            # mconcat
            # glueLine
            # wrapTrail
  where edge = iterTrail koch !! n

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > snowflake4 = snowflake 4 # strokeT # centerXY # pad 1.1

-- | A cool diagram featuring successive iterations of @sqUpDown'@
--   superimposed atop one another.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownOverlayD.svg#diagram=sqUpDownOverlayD&width=400>>
sqUpDownOverlay :: (RealFloat n, Typeable n) =>
                   Renderable (Path V2 n) b => Diagram b V2 n
sqUpDownOverlay
  = sized (Width 4)
  . mconcat
  . zipWith lc (iterate (blend 0.1 white) blue)
  . map strokeLine
  . take 5
  . iterTrail
  $ sqUpDown'

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > sqUpDownOverlayD = sqUpDownOverlay

--------------------------------------------------
-- Generating random iterated subset fractals

-- | Parameters to generate an iterated subset fractal.
data IterTrailConfig n = ITC { seed :: Trail' Line V2 n -- ^ The seed trail
                           , color  :: Colour Double  -- ^ The line color to use
                           , iters  :: Int            -- ^ Number of iterations
                           }

-- | Generate a random 'IterTrailConfig'.  This features many
--   hard-coded values.  If you want to play with it just copy the
--   code and modify it to suit.
randITC :: (MonadRandom m, Applicative m, Ord n, Floating n, Random n) =>
           m (IterTrailConfig n)
randITC = do
  -- use between two and five segments for the seed pattern
  nSegs <- getRandomR (2,5)

  -- should we make the seed pattern a cubic spline?
  spline  <- getRandom

  -- generate a random list of linear segments drawn from (-1,1)^2.
  s       <- fromOffsets <$>
                replicateM nSegs ((^&) <$> getRandomR (-1,1) <*> getRandomR (-1,1))

  -- generate a random color.
  c       <- sRGB <$> getRandom <*> getRandom <*> getRandom

  -- generate a random number of iterations, with a lower bound of 3
  -- (since fewer is not very interesting) and an upper bound chosen
  -- to ensure we won't get more than 10000 segments in the final
  -- path.
  i       <- getRandomR (3, floor (logBase (fromIntegral nSegs :: Double) 10000))
  let s'
        | spline    = cubicSpline False s
        | otherwise = fromVertices s
  return $ ITC s' c i

-- | Generate an iterated subset fractal based on the given parameters.
drawITC :: (Renderable (Path V2 n) b, RealFloat n, Ord n, Typeable n) =>
           IterTrailConfig n -> Diagram b V2 n
drawITC (ITC s c i) = (iterTrail s !! i) # strokeLine # lc c

-- | Like 'drawITC', but also scales, centers, and pads the result so
-- that it fits nicely inside a 4x4 box.
drawITCScaled
  :: (Renderable (Path V2 n) b, Backend b V2 n, RealFloat n, Typeable n)
  => IterTrailConfig n -> Diagram b V2 n
drawITCScaled itc
  = drawITC itc
  # sized (Dims 4 4)
  # centerXY
  # pad 1.1

-- | Create a grid of 100 random iterated subset fractals.  Impress
--   your friends!
randIterGrid :: (Renderable (Path V2 n) b, Backend b V2 n,
                 Random n, Typeable n, RealFloat n) =>
                IO (Diagram b V2 n)
randIterGrid = do
  itcs <- evalRandIO (replicateM 100 randITC)
  return (vcat . map hcat . chunksOf 10 . map drawITCScaled $ itcs)
