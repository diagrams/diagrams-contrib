-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Calligraphic
-- Copyright   :  (c) 2013 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Create \"calligraphic\" strokes by filling between two offset
-- copies of a curve.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.Calligraphic
    ( calligraphic
    ) where

import Diagrams.Prelude
-- import           Geometry
-- import Data.Semigroup
-- import Control.Lens
-- import Linear.Vector

-- | @calligraphic pen t@ creates a \"calligraphic\" variant of @t@ by
--   filling between two copies of @t@ offset by the @pen@ vector.
--
--   <<diagrams/src_Diagrams_TwoD_Path_Calligraphic_calligraphicEx.svg#diagram=calligraphicEx&width=400>>
--
--   > import Diagrams.Coordinates
--   > import Diagrams.TwoD.Path.Calligraphic
--   >
--   > curve = cubicSpline False [1 ^& 1, 2 ^& 5, 5 ^& 6, 8 ^& 12]
--   >       # scale 3
--   >
--   > calligraphicEx =
--   >   [ circle 15 # calligraphic (2 ^& 1)
--   >   , curve     # calligraphic (2 ^& 2)
--   >   ]
--   >   # map (centerXY . strokeLoop)
--   >   # fc black
--   >   # hcat' (with & sep .~ 10)

calligraphic :: (Floating n, Ord n) => V2 n -> Line V2 n -> Loop V2 n
calligraphic pen p
  = (p <> fromOffsets [pen] <> reversing p <> fromOffsets [negated pen])
  # closeLine
