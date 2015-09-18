{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Swimunit.Grid
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Base

{-
  Constructs the vertical grid without baseline.
 -}
verticgrid :: [Double]                        -- ^ List of y-values for gridlines.
           -> Double                          -- ^ Horizontal width covered by this grid.
           -> Diagram B                       -- ^ Resulting diagram.
verticgrid ylist wdth = (
             if (length ylist > 0)
             then position (zip (map (\y -> p2(0.0, y)) ylist) (repeat (hrule wdth)))
             else emptyd
        )

{-
  Constructs the horizontal grid without baseline.
 -}
horizgrid :: [Double]                         -- ^ List of x-values for gridlines.
          -> Double                           -- ^ Vertical height covered by this grid.
          -> Diagram B                        -- ^ Resulting diagram.
horizgrid xlist hght = (
             if (length xlist > 0)
             then position (zip (map (\x -> p2(x, 0.0)) xlist) (repeat (vrule hght)))
             else emptyd
        )


 --
----
 --
