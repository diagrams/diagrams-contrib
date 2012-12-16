-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.CirclePacking
-- Copyright   :  (c) 2012 Joachim Breitner
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mail@joachim-breitner.de
--
-- A method for layoung out diagrams using a circle packing algorithm. For
-- details on the algorithm, see "Optimisation.CirclePacking" in the module
-- circle-packing.

module Diagrams.TwoD.Layout.CirclePacking
       ( renderCirclePacking
       , RadiusFunction
       , approxRadius
       , circleRadius ) where

import Optimisation.CirclePacking

import Diagrams.Prelude
import Diagrams.Core.Envelope


-- | Combines the passed objects, whose radius is estimated using the given
-- 'RadiusFunction', so that they do not overlap (according to the radius
-- function) and otherwise form, as far as possible, a tight circle.
renderCirclePacking :: Monoid' m => RadiusFunction b m -> [QDiagram b R2 m] -> QDiagram b R2 m
renderCirclePacking radiusFunc =
    position .
    map (\(o,(x,y)) -> (p2 (x,y),o)) .
    packCircles radiusFunc

-- | The type of radius-estimating functions. If you have better information
-- about your diagrams, you can of course always pass your own function to
-- 'renderCirclePacking'. Otherwise, use 'approxRadius' or 'circleRadius'.
type RadiusFunction b m = QDiagram b R2 m -> Double

-- | A safe approximation. Calculates the outer radius of the smallest
-- axis-aligned polygon with the given number of edges that contains the
-- object. A parameter of 4 up to 8 should be sufficient for most applications.
approxRadius :: Int -> RadiusFunction b m
approxRadius n =
    if n < 3
    then error "circleRadius: n needs to be at least 3"
    else \o -> outByIn * maximum [ envelopeS (e alpha) o
                          | i <- [1..n]
                          , let alpha = CircleFrac ((fromIntegral i + 0.5) / fromIntegral n)
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
circleRadius :: RadiusFunction b m
circleRadius o = maximum [ envelopeS (e (CircleFrac alpha)) o | alpha <- [0,0.25,0.5,0.75]]

