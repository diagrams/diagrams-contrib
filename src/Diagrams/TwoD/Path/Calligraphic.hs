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

import           Diagrams.Prelude

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

calligraphic :: R2 -> Trail' Line R2 -> Trail' Loop R2
calligraphic pen p
  = (p <> fromOffsets [pen] <> reverseLine p <> fromOffsets [negateV pen])
  # closeLine
