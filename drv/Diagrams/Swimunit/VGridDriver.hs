{-# LANGUAGE NoMonomorphismRestriction #-}

{- |

-}
module Main
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Axis
import Diagrams.Swimunit.Grid

{-|
  MAIN method. Call 'main' to produce output.
 -}
main :: IO ()
main = mainWith (
        gridplot
        <>
        backgroundrect 1.1 gridplot
            # centerXY   -- center background
    )

gridplot :: Diagram B
gridplot = ( vplot
                # centerXY   -- center plot
           )

vplot :: Diagram B
vplot = (       verticlabel vml1 0.3
                       # fc aqua
                |||    (
                  verticticks vmt1 0.2 0.0
                       # lc lightblue
                       <>
                  verticticks vnt1 0.1 (-1.0)
                       # lc lightblue
                )
                |||
                verticgrid vmt1 8.2
                       # lc aqua
                |||
                verticticks vmt1 0.2 1.0
                       # lc lime
        )

vmt1 :: [Double]
vmt1 = [ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ]

{- Minor ticks. -}
vnt1 :: [Double]
vnt1 = [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
       , 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9
       , 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9
       , 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9 ]

vml1 :: [String]
vml1 = [ "2", "4", "6", "8" ]

backgroundrect :: Double
               -> Diagram B
               -> Diagram B
backgroundrect scl diagram = rect ((width diagram) * scl)
                                  ((height diagram) * scl)
                               # lc green
                               # fc black

 --
----
 --
