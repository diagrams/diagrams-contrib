-- {-# LANGUAGE CPP                   #-}
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
         -- $ventrella
       , GeneratorSegment(..), mkGS, mkGS3, Generator
       , refineGeneratorSegment, iterGenerator

         -- ** Utilities
       , averageLine
       , bevelLine

       -- , showGenerator

         -- * Examples

         -- ** Example seed trails
         -- $seeds

       , koch
       , levy
       , zag
       , sqUp
       , sqUpDown

         -- ** Example generators
         -- $gens

       , dragonGen
       , polyaGen
       , terDragonGen
       , invTerDragonGen
       , ventrella56b
       , yinDragonGen
       , ventrella67
       , innerFlipQuartetGen
       , antiGosperGen
       , mandelbrotSnowflakeGen

         -- ** Other stuff
         -- $other

       , snowflake

       , IterTrailConfig(..), randITC, drawITC, drawITCScaled
       , randIterGrid
       ) where

-- Diagrams.Core.Points needed for V (Point a) instance on GHC < 7.6
-- import           Diagrams.Core.Points       ()
import           Diagrams.Prelude

import           Control.Monad              (replicateM)
import           Control.Monad.Random       (evalRandIO, getRandom, getRandomR)
import           Control.Monad.Random.Class (MonadRandom)
import           Data.Bits                  (xor)
import           Data.Maybe                 (mapMaybe)
-- import           Data.Typeable
import           System.Random              (Random)

import qualified Diagrams.TwoD.Layout.Grid  as LG

------------------------------------------------------------
-- Iterated subset algorithm (simplified version)
------------------------------------------------------------

lineSegments :: Line v n -> [Segment v n]
lineSegments = toListOf segments

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
--   > iterTrailEx = vsep 0.3 . map strokeLine . take 5
--   >             $ iterTrail koch
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_iterTrailEx.svg#diagram=iterTrailEx&width=200>>
iterTrail :: RealFloat n => Line V2 n -> [Line V2 n]
iterTrail seed' = iterate (\tr -> mconcat . mapMaybe (refineSegment tr) $ offs)
                         (fromOffsets [unitX])
  where offs = map offset . lineSegments $ seed'

-- | Use a trail to \"refine\" a linear segment (represented by a
--   vector), returning a scaled and/or rotated copy of the trail with
--   the same endpoint as the segment.
refineSegment :: RealFloat n =>
                 Line V2 n -> V2 n ->
                 Maybe (Line V2 n)
refineSegment t sOff
  | tOff == zero || sOff == zero = Nothing
  | otherwise                    = Just $ t # scale k # rotate r
  where
    tOff = offset t
    k    = norm sOff / norm tOff
    r    = (sOff^._theta) ^-^ (tOff^._theta)

------------------------------------------------------------
-- Generators
------------------------------------------------------------

-- $ventrella
-- Much of the approach here is taken from Jeffrey Ventrella,
-- /Brain-filling Curves/. EyeBrain Books, 2nd edition, 2012. ISBN
-- 9780983054627. <http://www.fractalcurves.com/>,
-- <http://www.brainfillingcurves.com/>
--
-- Each generator consists of a sequence of linear segments with
-- endpoints on a square or triangular grid.  Each segment can also
-- have one of four orientations which determines how it is replaced
-- by a copy of the entire fractal path.  Generators are classified by
-- the distance between their start and end points; generators for
-- which the sum of the squared lengths of their segments is equal to
-- the square of this overall distance have fractal dimension 2 and
-- thus are candidates to be space-filling curves.

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

-- generatorSegmentOffset :: GeneratorSegment n -> V2 n
-- generatorSegmentOffset (GS v _ _) = v

-- generatorToLine :: (Floating n, Ord n) => Generator n -> Line V2 n
-- generatorToLine = fromOffsets . map generatorSegmentOffset

-- | Create a graphical representation of a generator, using half
--   arrowheads to show the orientation of each segment.
-- showGenerator
--   :: Generator Double -> Diagram V2
-- showGenerator g = mconcat $ zipWith showGenSeg (explodeTrail (OpenTrail (generatorToLine g) `at` origin)) g
--   where
--     showGenSeg
--       :: Located (Trail V2 Double) -> GeneratorSegment Double -> Diagram V2
--     showGenSeg locTr (GS _ flip1 _) =
--       let locTr' = if flip1 then reversing locTr else locTr
--       in  arrowAt' (with & arrowHead .~ halfDart) (loc locTr') (trailOffset (unLoc locTr'))


-- | Make a generator segment by specifying an x component, a y
--   component, a \"horizontal\" orientation (1 means normal, -1 means
--   reversing the start and end of the segment) and a \"vertical\"
--   orientation (1 means normal, -1 means reflecting across the axis
--   of the segment).  This corresponds to the notation used by
--   Ventrella in /Brainfilling Curves/.
mkGS :: (n, n, Int, Int) -> GeneratorSegment n
mkGS (x, y, flip1, flip2)
  = mkGSv (r2 (x,y)) flip1 flip2

-- | Make a generator segment on a triangular grid, by specifying a
--   segment on a square grid and then applying a shear and a scale to
--   transform the square grid into a triangular grid, as in the
--   diagram below:
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_gridTransform.svg#diagram=gridTransform&width=400>>
mkGS3 :: Floating n => (n, n, Int, Int) -> GeneratorSegment n
mkGS3 (x, y, flip1, flip2)
  = mkGSv (r2 (x,y) # shearX 0.5 # scaleY (sqrt 3 / 2)) flip1 flip2

-- > sqGrid =
-- >   mconcat
-- >   [ vec (-1) 1
-- >   , vec 0 1
-- >   , vec 1 0
-- >   , vec 1 (-1)
-- >   , vec 0 (-1)
-- >   , vec (-1) 0
-- >   , replicate 4 (hcat $ replicate 4 (square 1)) # vcat # centerXY # lw thin # lc gray
-- >   ]
-- >   where
-- >     vec x y = arrowAt origin (x ^& y)
-- >
-- > gridTransform = hcat' (with & catMethod .~ Distrib & sep .~ 3.5)
-- >   [ sqGrid
-- >   , arrow 2 # lc blue # centerX
-- >   , sqGrid # shearX 0.5 # scaleY (sqrt 3 / 2)
-- >   ]

-- | General interface used by both mkGS3 and mkGS.
mkGSv :: V2 n -> Int -> Int -> GeneratorSegment n
mkGSv v flip1 flip2 = GS v (flip1 < 0) (flip2 < 0)

-- | Use a trail to \"refine\" a generator segment, returning a scaled
--   and/or rotated copy of the trail with the same endpoints as the
--   segment, and with appropriate reflections applied depending on
--   the orientation of the segment.
refineGeneratorSegment :: RealFloat n =>
                          Line V2 n -> GeneratorSegment n ->
                          Maybe (Line V2 n)
refineGeneratorSegment t (GS sOff flipX flipY)
  = refineSegment (t # doFlips flipX flipY) sOff

doFlips :: (Ord n, Floating n) => Bool -> Bool -> Line V2 n -> Line V2 n
doFlips flipX flipY
  = (if flipX `xor` flipY then reflectY else id)
  . (if flipX then reversing else id)

{-
doFlips flipX flipY
  = (if flipY then reflectY else id) . (if flipX then (reflectY . reverseLine) else id)

   flipX / flipY   T         F
        T     reverseLine    reflectY . reverseLine
        F      reflectY      id

   flipX `xor` flipY -> reflectY
   flipX -> reverseLine
-}

-- | Given a generator, produce a list of successive refinements: the
--   zeroth trail in the output list is a horizontal unit segment, and
--   the nth trail is formed by refining each segment of the generator
--   with the (n-1)st trail.
--
--   > import Diagrams.TwoD.Path.IteratedSubset
--   > iterGenEx = hsep 0.3 . map strokeLine . take 7
--   >           $ iterGenerator dragonGen
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_iterGenEx.svg#diagram=iterGenEx&width=400>>
iterGenerator :: RealFloat n => Generator n -> [Line V2 n]
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
--
-- All these seed trails can be seen as generators with homogeneous
-- orientation.  For more complex/interesting generators, see the
-- section of generators below.

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > showTrail n t = hcat' (with & sep .~ 0.2) [ iters !! 1, iters !! n ]
-- >             # frame 0.5
-- >   where iters = map strokeLine $ iterTrail t

-- | Seed for the Koch curve (side of the famous Koch 'snowflake').
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_kochD.svg#diagram=kochD&width=400>>
koch :: (InSpace V2 n t, Floating n, FromTrail t) => t
koch = fromOffsets [unitX, unitX # rotateBy (1/6), unitX # rotateBy (-1/6), unitX]

-- > kochD = showTrail 4 koch

-- | Seed for the Lévy dragon curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_levyD.svg#diagram=levyD&width=400>>
levy :: (InSpace V2 n t, FromTrail t) => t
levy = fromOffsets [unitY, unitX]

-- > levyD = showTrail 9 levy

-- | Strange zig-zag seed that produces a dense fractal path with lots
--   of triangles.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_zagD.svg#diagram=zagD&width=400>>
zag :: (InSpace V2 n t, FromTrail t, Fractional n) => t
zag = fromOffsets [unitX, V2 (-0.5) 1, unitX]

-- > zagD = showTrail 5 zag

-- | A \"square impulse\" seed which produces a quadratic von Koch
--   curve.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpD.svg#diagram=sqUpD&width=400>>
sqUp :: (InSpace V2 n t, FromTrail t) => t
sqUp = fromOffsets [unitX, unitY, unitX, unit_Y, unitX]

-- > sqUpD = showTrail 3 sqUp

-- | A \"double square impulse\" seed which produces fantastic
--   rectilinear spiral patterns.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_sqUpDownD.svg#diagram=sqUpDownD&width=400>>
sqUpDown :: (InSpace V2 n t, FromTrail t) => t
sqUpDown = fromOffsets [unitX, unitY, unitX, 2 *^ unit_Y, unitX, unitY, unitX]

-- > sqUpDownD = showTrail 3 sqUpDown

--------------------------------------------------
-- Example generators
--------------------------------------------------

-- $gens
-- Many of these generators are taken from Jeffrey Ventrella,
-- /Brain-filling Curves/, which has a large number of other examples
-- as well (see <http://www.brainfillingcurves.com/>).

-- > import Diagrams.TwoD.Path.IteratedSubset
-- >
-- > illustrateGen k g
-- >     = hcat' (with & sep .~ 0.2) [ showGenerator g, iters !! 2, iters !! k ]
-- >     # frame 0.5
-- >   where iters = map strokeLine $ iterGenerator g

-- | Generator for the classic Harter-Heighway Dragon (Ventrella
--   p. 52, sqrt 2 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_dragonD.svg#diagram=dragonD&width=400>>
dragonGen :: Generator Double
dragonGen
  = map mkGS
    [ (1, 0,  1,  1)
    , (0, 1, -1, -1)
    ]

-- > dragonD = illustrateGen 7 dragonGen

-- | Generator for the Pólya sweep (Ventrella p. 52, sqrt 2 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_polyaD.svg#diagram=polyaD&width=400>>
polyaGen :: Generator Double
polyaGen
  = map mkGS
    [ (1, 0,  1, -1)
    , (0, 1, -1,  1)
    ]

-- > polyaD = illustrateGen 7 polyaGen

-- | Generator for the Ter-Dragon (Ventrella p. 55, sqrt 3 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_terDragonD.svg#diagram=terDragonD&width=400>>
terDragonGen :: Generator Double
terDragonGen
  = map mkGS3
    [ (0,  1, 1, 1)
    , (1, -1, 1, 1)
    , (0,  1, 1, 1)
    ]

-- > terDragonD = illustrateGen 5 terDragonGen

-- | Inverted Ter-Dragon (Ventrella p. 56, sqrt 3 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_invTerDragonD.svg#diagram=invTerDragonD&width=400>>
invTerDragonGen :: Generator Double
invTerDragonGen
  = map mkGS3
    [ (0,  1, -1, 1)
    , (1, -1, -1, 1)
    , (0,  1, -1, 1)
    ]

-- > invTerDragonD = illustrateGen 5 invTerDragonGen

-- | Ventrella p. 56b, sqrt 3 family.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_ventrella56bD.svg#diagram=ventrella56bD&width=400>>
ventrella56b :: Generator Double
ventrella56b
  = map mkGS3
    [ (0,  1, -1, 1)
    , (1, -1,  1, 1)
    , (0,  1, -1, 1)
    ]

-- > ventrella56bD = illustrateGen 5 ventrella56b

-- | Yin Dragon (Ventrella p. 59, sqrt 3 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_yinDragonD.svg#diagram=yinDragonD&width=400>>
yinDragonGen :: Generator Double
yinDragonGen
  = map mkGS3
    [ (0,  1,  1,  1)
    , (0,  1, -1, -1)
    , (1, -1,  1,  1)
    ]

-- > yinDragonD = illustrateGen 6 yinDragonGen

-- | Ventrella p. 67, sqrt 4 family.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_ventrella67D.svg#diagram=ventrella67D&width=400>>
ventrella67 :: Generator Double
ventrella67
  = map mkGS
    [ (1,  1, -1, -1)
    , (1,  0,  1,  1)
    , (0, -1,  1,  1)
    ]

-- > ventrella67D = illustrateGen 6 ventrella67

-- | "Inner-flip Quartet" (Ventrella p. 85, sqrt 5 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_innerFlipQuartetD.svg#diagram=innerFlipQuartetD&width=600>>
innerFlipQuartetGen :: Generator Double
innerFlipQuartetGen
  = map mkGS
    [ (0,  1,  1, -1)
    , (0,  1, -1,  1)
    , (1,  0, -1,  1)
    , (0, -1,  1, -1)
    , (1,  0, -1,  1)
    ]

-- > innerFlipQuartetD = illustrateGen 5 innerFlipQuartetGen

-- | \"Anti-Gosper\" (Ventrella p. 97, sqrt 7 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_antiGosperD.svg#diagram=antiGosperD&width=600>>
antiGosperGen :: Generator Double
antiGosperGen
  = map mkGS3
    [ ( 1,  0,  1,  1)
    , ( 0,  1,  1,  1)
    , (-1,  0, -1, -1)
    , (-1,  1, -1, -1)
    , ( 1,  0,  1,  1)
    , ( 1,  0, -1, -1)
    , ( 1, -1, -1, -1)
    ]

-- > antiGosperD = illustrateGen 4 antiGosperGen

-- | "Mandelbrot Snowflake Sweep #2" (Ventrella p. 197, sqrt 27 family).
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_mandelSnowD.svg#diagram=mandelSnowD&width=600>>
mandelbrotSnowflakeGen :: Generator Double
mandelbrotSnowflakeGen
  = map mkGS3
    [ (-1,  2,  1, -1)
    , (-1,  2,  1,  1)
    , ( 1,  1,  1,  1)
    , ( 2, -1,  1,  1)
    , (-1,  0,  1,  1)
    , (-1,  0,  1, -1)
    , ( 0, -1,  1, -1)
    , ( 1, -1,  1, -1)
    , ( 1,  1,  1,  1)
    , ( 0, -1,  1,  1)
    , ( 0, -1,  1, -1)
    , ( 1,  1,  1, -1)
    , ( 1,  1,  1,  1)
    ]

-- > mandelSnowD = illustrateGen 3 mandelbrotSnowflakeGen

-- | Perform a \"level-1 smoothing\" by replacing a list of segments by
--   the segments between their midpoints.  Can be a useful technique
--   for visualizing degenerate space-filling curves, /e.g./ which
--   touch at corners or even share entire edges.
averageLine :: (Metric v, Floating n, Ord n) => Line v n -> Line v n
averageLine = fromOffsets . (zipWith (lerp 0.5) <*> tail) . map offset . lineSegments

-- | Bevel a line by \"chopping off each corner\", connecting points 1/3
--   and 2/3 of the way along each segment.  Can be a useful technique
--   for visualizing degenerate space-filling curves, /e.g./ which touch
--   at corners or even share entire edges.
bevelLine :: (Metric v, Floating n, Ord n) => Line v n -> Line v n
bevelLine = fromOffsets . concat . (zipWith (\v1 v2 -> [v1 ^/ 3, (v1 ^+^ v2) ^/ 3]) <*> tail) . map offset . lineSegments

--------------------------------------------------
-- Miscellaneous examples

-- $other
-- A random collection of other fun things you can do with 'iterTrail'
-- or 'iterGenerator'.  There is no particular emphasis on making
-- these configurable or generic; the point is just to suggest some
-- fun things you can do.  If you want to play with them, copy the
-- source code and modify it as you see fit.

-- | The famous Koch snowflake, made by putting three Koch curves
--   together. @snowflake n@ yields an order-@n@ snowflake.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_snowflake4.svg#diagram=snowflake4&width=300>>
snowflake :: RealFloat n => Int -> Trail V2 n
snowflake n = iterateN 3 (rotateBy (-1/3)) edge
            # mconcat
            # glueLine
            # ClosedTrail

  where edge = iterTrail koch !! n

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > snowflake4 = snowflake 4 # strokeT # centerXY # pad 1.1

--------------------------------------------------
-- Generating random iterated subset fractals

-- | Parameters to generate an iterated subset fractal.
data IterTrailConfig n = ITC { seed :: Line V2 n -- ^ The seed trail
                           , color  :: Colour Double  -- ^ The line color to use
                           , iters  :: Int            -- ^ Number of iterations
                           }

-- | Generate a random 'IterTrailConfig'.  This features many
--   hard-coded values.  If you want to play with it just copy the
--   code and modify it to suit.
randITC ::
  (MonadRandom m,
   Ord n, Floating n, Random n) =>
           m (IterTrailConfig n)
randITC = do
  -- use between two and five segments for the seed pattern
  nSegs   <- getRandomR (2,5)

  -- should we make the seed pattern a cubic spline?
  spline  <- getRandom

  -- generate a random list of linear segments drawn from (-1,1)^2.
  s       <- fromOffsets <$>
                replicateM nSegs (V2 <$> getRandomR (-1,1) <*> getRandomR (-1,1))

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
drawITC :: IterTrailConfig Double -> Diagram V2
drawITC (ITC s c i) = (iterTrail s !! i) # stroke # lc c

-- | Like 'drawITC', but also scales, centers, and pads the result so
-- that it fits nicely inside a 4x4 box.
drawITCScaled
  :: IterTrailConfig Double -> Diagram V2
drawITCScaled itc
  = drawITC itc
  # sized (dims2D 4 4)
  # centerXY
  # pad 1.1

-- | Create a grid of 25 random iterated subset fractals.  Impress
--   your friends!
--
-- <<diagrams/src_Diagrams_TwoD_Path_IteratedSubset_randIterGridEx.svg#diagram=randIterGridEx&width=500>>
randIterGrid :: IO (Diagram V2)
randIterGrid = do
  itcs <- evalRandIO (replicateM 25 randITC)
  return (LG.gridCat . map drawITCScaled $ itcs)

-- > import Diagrams.TwoD.Path.IteratedSubset
-- > randIterGridEx = randIterGrid
