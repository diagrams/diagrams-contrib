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
import           Data.List                (minimumBy)
import           Data.Ord                 (comparing)

-- | Blend two colors in HSV space---that is, linearly interpolate
--   between their hues, saturations, and values independently
--   (wrapping around appropriately in the case of hue).  In
--   particular, @hsvBlend a c1 c2@ is like doing @(a-1)*c1 + a*c2@ in
--   HSV space.  That is, a parameter of @0@ results in only the first
--   color; @1@ results in only the second color; and anything in
--   between results in a blend.
hsvBlend :: RealFloat n => n -> Colour n -> Colour n -> Colour n
hsvBlend t c1 c2 = uncurryRGB sRGB . hsv3
                 $ (lerpWrap h1 h2 360 t, lerp' s1 s2 t, lerp' v1 v2 t)
  where
    [(h1,s1,v1), (h2,s2,v2)] = map (hsvView . toSRGB) [c1,c2]
    hsv3 (h,s,v) = hsv h s v

lerpWrap :: (RealFrac n) => n -> n -> n -> n -> n
lerpWrap a b m t = lerp' a b' t `dmod` m
  where
    b' = minimumBy (comparing (abs . subtract a)) [b - m, b, b + m]

-- | Interpolate linearly between two values.  The first argument is
-- the parameter.  A parameter of @0@ results in the second argument;
-- with a parameter of @1@, @lerp'@ returns its third argument.
lerp' :: Num n => n -> n -> n -> n
lerp' t a b = t * a + (1 - t) * b

dmod :: RealFrac n => n -> n -> n
dmod a m = a - m * fromIntegral (floor (a/m) :: Integer)
