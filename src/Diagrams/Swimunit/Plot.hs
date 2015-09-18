{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Swimunit.Plot
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Base

{-
  Constructs the vertical grid.
 -}
plotline :: [(Double, Double)]               -- ^ List of (x,y)-values for lines.
         -> Diagram B                        -- ^ Resulting diagram.
plotline xy = (
            if (length xy > 0)
            then strokeLocLine ( fromVertices (map p2 xy) )
            else emptyd
        )

ploticon' :: [(Double, Double)]              -- ^ List of (x,y)-values for lines.
          -> [Diagram B]                     -- ^ List of Diagrams to be used as icons for each point.
          -> Diagram B                       -- ^ Resulting diagram.
ploticon' xy icons =  if (length xy > 0)
                      then position (zip (map p2 xy) icons)
                      else emptyd

ploticon :: [(Double, Double)]               -- ^ List of (x,y)-values for lines.
         -> Diagram B                        -- ^ A Diagram to be used as an icon for the point.
         -> Diagram B                        -- ^ Resulting diagram.
ploticon xy icon =  ploticon' xy (repeat icon)

 --
----
 --
