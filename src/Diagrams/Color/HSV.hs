-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Color.HSV
-- Copyright   :  (c) 2013 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Utilities for working with color in HSV space.
--
-- Right now, the only utility is a function for blending colors in
-- HSV space.  This has the effect of \"travelling around the color
-- wheel\", which can be especially nice when one wants to blend
-- smoothly from one color to another (blending in RGB space can tend
-- to travel across some icky brown/grey colors).
--
-----------------------------------------------------------------------------

module Diagrams.Color.HSV
       ( hsvBlend
       ) where

import           Data.Colour              (Colour)
import           Data.Colour.RGBSpace     (uncurryRGB)
import           Data.Colour.RGBSpace.HSV (hsv, hsvView)
import           Data.Colour.SRGB         (sRGB, toSRGB)
import           Data.VectorSpace         (Scalar, VectorSpace, lerp)

-- | Blend two colors in HSV space---that is, linearly interpolate
--   between their hues, saturations, and values independently
--   (wrapping around appropriately in the case of hue).  In
--   particular, @hsvBlend a c1 c2@ is like doing @(a-1)*c1 + a*c2@ in
--   HSV space.  That is, a parameter of @0@ results in only the first
--   color; @1@ results in only the second color; and anything in
--   between results in a blend.
hsvBlend :: (Floating a, RealFrac a, VectorSpace a)
         => Scalar a -> Colour a -> Colour a -> Colour a
hsvBlend t c1 c2 = uncurryRGB sRGB . hsv3 $ (lerpWrap h1 h2 360 t, lerp s1 s2 t, lerp v1 v2 t)
  where
    [(h1,s1,v1), (h2,s2,v2)] = map (hsvView . toSRGB) [c1,c2]
    hsv3 (h,s,v) = hsv h s v

lerpWrap a b m t
  | (d_ab <= d_ba && a <= b) || (d_ba <= d_ab && b <= a) = lerp a b t
  | a < b     = lerp (a + m) b t `dmod` m
  | otherwise = lerp a (b + m) t `dmod` m
  where
    d_ab = (b - a) `dmod` m
    d_ba = (a - b) `dmod` m

dmod a m = a - m * fromIntegral (floor (a/m))
