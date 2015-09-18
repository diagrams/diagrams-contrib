{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Swimunit.Axis
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Base

{-|
  Constructs the vertical baseline of height height.
 -}
verticaxis :: Double
           -> Diagram B
verticaxis hght = vrule hght
                        -- Setting line color here would override color setting of resulting diagram.
                        -- # lc lime
                        # alignX (-1.00)
                        # alignY (-1.00)

{-|
  Constructs the horizontal baseline of width width.
 -}
horizaxis :: Double
          -> Diagram B
horizaxis wdth = hrule wdth
                    -- Setting line color here would override color setting of resulting diagram.
                    -- # lc lime
                    # alignX (-1.00)
                    # alignY (-1.00)

{-|
  Constructs horizontal ticks for the vertical axis.
 -}
verticticks :: [Double]                       -- ^ List of y-values for ticks.
            -> Double                         -- ^ Horizontal width covered by each tick.
            -> Double                         -- ^ -1.0 fully in ... 0.0 centered ... +1.0 fully out.
            -> Diagram B                      -- ^ Resulting diagram.
verticticks ylist wdth inout = (
              if (length ylist > 0)
              then position (zip (map (\y -> p2(0.0, y)) ylist)
                                 (repeat (hrule wdth # alignX inout))
                            )
              else emptyd
        )

{-
  Constructs vertical labels.
 -}
verticlabel :: [String]                        -- ^ List of y-labels.
            -> Double                          -- ^ Text size.
            -> Diagram B                       -- ^ Resulting diagram.
verticlabel ylabel sze = (
                verticlabel' ylabel                                -- list of y-labels
                             (map (\s -> p2(0.0, read s)) ylabel)  -- list of P2-positions from y-labels
                             sze                                   -- size
            )

{-|
  Constructs vertical labels from position and string.
 -}
verticlabel' :: [String]                        -- ^ List of y-labels.
             -> [P2 Double]                     -- ^ List of positions.
             -> Double                          -- ^ Text size.
             -> Diagram B                       -- ^ Resulting diagram.
verticlabel' ylabel xypos sze = (
                if (length ylabel > 0)
                then position (zip xypos                                         -- list of P2-positions
                                   (map (\s -> (verticlabeltext s sze)) ylabel)  -- list of labeling diagrams
                              )
                else emptyd
            )

{-|
  Constructs the visual representation of one vertical grid label.
 -}
verticlabeltext :: String                       -- ^ List of y-labels.
                -> Double                       -- ^ Text size.
                -> Diagram B                    -- ^ Resulting diagram.
verticlabeltext label sze = (
                    alignedText 1.0 0.5 label
                        # scale sze
                    <>
                    hrule sze
                )


 --
----
 --
