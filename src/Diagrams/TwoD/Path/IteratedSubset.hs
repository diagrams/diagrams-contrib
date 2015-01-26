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
         -- ** Simplified version
         refineSegment, iterTrail

         -- ** General version
       , GeneratorSegment(..), mkGS, Generator
       , refineGeneratorSegment, iterGenerator

         -- * Examples

         -- ** Example seed trails
         -- $seeds

       , koch
       , levy
       , zag
       , sqUp
       , sqUpDown
       , sqUpDown'

         -- ** Example generators
         -- $gens

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
-- Iterated subset algorithm (simplified version)
------------------------------------------------------------

-- | Given a \"seed pattern\", produce a list of successive
--   refinements: the zeroth trail in the output list is a horizontal
--   unit segment, and the nth trail is formed by replacing each
--   segment of the seed pattern with the (n-1)st trail.
--   (Equivalently, the nth trail consists of the (n-1)st trail with
--   every segment replaced by the seed pattern.)
--
--   See 'iterGenerator' for a more sophisticated variant which can
--   associate one of four orientations with each segment of the seed
--   pattern.
--
--   > import Diagrams.TwoD.Path.IteratedSubset
--   > iterTrailEx = vcat' (with & sep .~ 0.3) . map strokeLine . take 5
--   >             $ iterTrail koch
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_iterTrailEx.svg#diagram=iterTrailEx&width=200>>
iterTrail :: RealFloat n => Trail' Line V2 n -> [Trail' Line V2 n]
iterTrail seed = iterate (\tr -> mconcat . mapMaybe (refineSegment tr) $ offs)
                         (fromOffsets [unitX])
  where offs = map segOffset . lineSegments $ seed

-- | Use a trail to \"refine\" a linear segment (represented by a
--   vector), returning a scaled and/or rotated copy of the trail with
--   the same endpoint as the segment.
refineSegment :: RealFloat n =>
                 Trail' Line V2 n -> V2 n ->
                 Maybe (Trail' Line V2 n)
refineSegment t sOff
  | tOff == zero || sOff == zero = Nothing
  | otherwise                    = Just $ t # scale k # rotate r
  where
    tOff = lineOffset t
    k    = norm sOff / norm tOff
    r    = (sOff^._theta) ^-^ (tOff^._theta)

------------------------------------------------------------
-- Generators
------------------------------------------------------------

-- XXX TODO
--   - Finish commenting.  Cite Ventrella.
--   - Transcribe a bunch of generators
--   - Utilities for rounding corners etc., as in Ventrella

-- $gens
-- See Brain-filling XXX

-- | A /generator segment/ is a vector along with two bits' worth of
--   orientation information: whether there is a reflection swapping
--   its start and end, and whether there is a reflection across its
--   axis.  When a generator segment is replaced by a complex path,
--   the endpoints of the path will match the endpoints of the
--   segment, but the path may first have some reflections applied to
--   it according to the orientation of the segment.
data GeneratorSegment n = GS (V2 n) Bool Bool

-- | A generator is a sequence of consecutive generator segments.
type Generator n = [GeneratorSegment n]

-- | Make a generator segment by specifying an x component, a y
--   component, a \"horizontal\" orientation (1 means normal, -1 means
--   reversing the start and end of the segment) and a \"vertical\"
--   orientation (1 means normal, -1 means reflecting across the axis
--   of the segment).  This corresponds to the notation used by
--   Ventrella in XXX.
mkGS :: (n, n, Int, Int) -> GeneratorSegment n
mkGS (x, y, flip1, flip2) = GS (r2 (x,y)) (flip1 < 0) (flip2 < 0)

-- | Use a trail to \"refine\" a generator segment, returning a scaled
--   and/or rotated copy of the trail with the same endpoints as the
--   segment, and with appropriate reflections applied depending on
--   the orientation of the segment.
refineGeneratorSegment :: RealFloat n =>
                          Trail' Line V2 n -> GeneratorSegment n ->
                          Maybe (Trail' Line V2 n)
refineGeneratorSegment t (GS sOff flipX flipY) = refineSegment t' sOff
  where
    t' = t
       # (if flipY then reflectY else id)
       # (if flipX then (reflectY . reverseLine) else id)

-- | Given a generator, produce a list of successive refinements: the
--   zeroth trail in the output list is a horizontal unit segment, and
--   the nth trail is formed by refining each segment of the generator
--   with the (n-1)st trail.
--
--   > import Diagrams.TwoD.Path.IteratedSubset
--   > iterTrailEx = vcat' (with & sep .~ 0.3) . map strokeLine . take 5
--   >             $ iterTrail koch
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_iterTrailEx.svg#diagram=iterTrailEx&width=200>>
iterGenerator :: RealFloat n => Generator n -> [Trail' Line V2 n]
iterGenerator g = iterate (\tr -> mconcat . mapMaybe (refineGeneratorSegment tr) $ g)
                          (fromOffsets [unitX])

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
koch :: (TrailLike t, V t ~ V2, N t ~ n, Floating n) => t
koch = fromOffsets [unitX, unitX # rotateBy (1/6), unitX # rotateBy (-1/6), unitX]

-- > kochD = showTrail 4 koch

-- | Seed for the Lévy dragon curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_levyD.svg#diagram=levyD&width=400>>
levy :: (TrailLike t, V t ~ V2, N t ~ n) => t
levy = fromOffsets [unitY, unitX]

-- > levyD = showTrail 9 levy

-- | Strange zig-zag seed that produces a dense fractal path with lots
--   of triangles.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_zagD.svg#diagram=zagD&width=400>>
zag :: (TrailLike t, V t ~ V2, N t ~ n) => t
zag = fromOffsets [unitX, (-0.5) ^& 1, unitX]

-- > zagD = showTrail 5 zag

-- | A \"square impulse\" seed which produces a quadratic von Koch
--   curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpD.svg#diagram=sqUpD&width=400>>
sqUp :: (TrailLike t, V t ~ V2, N t ~ n) => t
sqUp = fromOffsets [unitX, unitY, unitX, unit_Y, unitX]

-- > sqUpD = showTrail 3 sqUp

-- | A \"double square impulse\" seed which produces fantastic
--   rectilinear spiral patterns.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownD.svg#diagram=sqUpDownD&width=400>>
sqUpDown :: (TrailLike t, V t ~ V2, N t ~ n) => t
sqUpDown = fromOffsets [unitX, unitY, unitX, 2 *^ unit_Y, unitX, unitY, unitX]

-- > sqUpDownD = showTrail 3 sqUpDown

-- | Like 'sqUpDown' but with 'cubicSpline' applied to produce a curvy
--   version.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownD2.svg#diagram=sqUpDownD2&width=400>>
sqUpDown' :: (TrailLike t, V t ~ V2, N t ~ n) => t
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
snowflake :: RealFloat n => Int -> Trail V2 n
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
sqUpDownOverlay :: (TypeableFloat n, Renderable (Path V2 n) b)
                => QDiagram b V2 n Any
sqUpDownOverlay
  = sized (mkWidth 4)
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
drawITC :: (Renderable (Path V2 n) b, TypeableFloat n) =>
           IterTrailConfig n -> QDiagram b V2 n Any
drawITC (ITC s c i) = (iterTrail s !! i) # strokeLine # lc c

-- | Like 'drawITC', but also scales, centers, and pads the result so
-- that it fits nicely inside a 4x4 box.
drawITCScaled
  :: (Renderable (Path V2 n) b, RealFloat n, Typeable n)
  => IterTrailConfig n -> QDiagram b V2 n Any
drawITCScaled itc
  = drawITC itc
  # sized (dims2D 4 4)
  # centerXY
  # pad 1.1

-- | Create a grid of 100 random iterated subset fractals.  Impress
--   your friends!
randIterGrid :: (Renderable (Path V2 n) b, Random n, TypeableFloat n) =>
                IO (QDiagram b V2 n Any)
randIterGrid = do
  itcs <- evalRandIO (replicateM 100 randITC)
  return (vcat . map hcat . chunksOf 10 . map drawITCScaled $ itcs)
