-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.CirclePacking
-- Copyright   :  (c) 2012 Joachim Breitner
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mail@joachim-breitner.de
--
-- A method for laying out diagrams using a circle packing algorithm. For
-- details on the algorithm, see "Optimisation.CirclePacking" in the module
-- circle-packing.
--
-- Here is an example:
--
-- > import Optimisation.CirclePacking
-- >
-- > colorize = zipWith fc $
-- >     cycle [red,blue,yellow,magenta,cyan,bisque,firebrick,indigo]
-- >
-- > objects = colorize $
-- >     [ circle r  | r <- [0.1,0.2..1.6] ] ++
-- >     [ hexagon r | r <- [0.1,0.2..0.7] ] ++
-- >     [ decagon r | r <- [0.1,0.2..0.7] ]
-- >
-- > -- Just a approximation, diagram objects do not have an exact radius
-- > radiusApproximation o = maximum [ radius (e (alpha :: Turn)) o | alpha <- [0,0.1..1.0]]
-- >
-- > circlePackingExample =
-- >     position $ map (\(o,(x,y)) -> (p2 (x,y),o)) $
-- >     packCircles radiusApproximation objects
--
-- <<diagrams/src_Diagrams_TwoD_Layout_CirclePacking_circlePackingExample.svg#diagram=circlePackingExample&width=400>>

module Diagrams.TwoD.Layout.CirclePacking
       ( renderCirclePacking
       , createCirclePacking
       , RadiusFunction
       , approxRadius
       , circleRadius ) where

import           Optimisation.CirclePacking

import           Diagrams.Core.Envelope
import           Diagrams.Prelude
import           Diagrams.TwoD.Vector       (e)


-- | Combines the passed objects, whose radius is estimated using the given
-- 'RadiusFunction', so that they do not overlap (according to the radius
-- function) and otherwise form, as far as possible, a tight circle.
renderCirclePacking :: Monoid' m => RadiusFunction b m -> [QDiagram b R2 m] -> QDiagram b R2 m
renderCirclePacking radiusFunc = createCirclePacking radiusFunc id

-- | More general version of 'renderCirclePacking'. You can use this if you
-- have more information available in the values of type @a@ that allows you to
-- calculate the radius better (or even exactly).
createCirclePacking :: Monoid' m => (a -> Double) -> (a -> QDiagram b R2 m) -> [a] -> QDiagram b R2 m
createCirclePacking radiusFunc diagramFunc =
    position .
    map (\(o,(x,y)) -> (p2 (x,y), diagramFunc o)) .
    packCircles radiusFunc

-- | The type of radius-estimating functions for Diagrams such as
-- 'approxRadius' and 'circleRadius'. When you can calculate the radius better,
-- but not any more once you converted your data to a diagram, use 'createCirclePacking'.
type RadiusFunction b m = QDiagram b R2 m -> Double

-- | A safe approximation. Calculates the outer radius of the smallest
-- axis-aligned polygon with the given number of edges that contains the
-- object. A parameter of 4 up to 8 should be sufficient for most applications.
approxRadius :: (Monoid' m) => Int -> RadiusFunction b m
approxRadius n =
    if n < 3
    then error "circleRadius: n needs to be at least 3"
    else \o -> outByIn * maximum [ envelopeS (e alpha) o
                          | i <- [1..n]
                          , let alpha :: Turn
                                alpha = (fromIntegral i + 0.5) / fromIntegral n
                          ]
    -- incircle radius: a / (2 * tan (tau/n))
    -- outcircle radius: a / (2 * sin (tau /n))
    -- hence factor is : out/in = tan (tau/n) / sin (tau/n)
  where
    outByIn = Prelude.tan (pi / (2 * fromIntegral n)) / sin (pi / (2 * fromIntegral n))
--
-- | An unsafe approximation. This is the radius of the largest circle that
-- fits in the rectangular bounding box of the object, so it may be too small.
-- It is, however, exact for circles, and there is no function that is safe for
-- all diagrams and exact for circles.
circleRadius :: (Monoid' m) => RadiusFunction b m
circleRadius o = maximum [ envelopeS (e (alpha :: Turn)) o | alpha <- [0,0.25,0.5,0.75]]
