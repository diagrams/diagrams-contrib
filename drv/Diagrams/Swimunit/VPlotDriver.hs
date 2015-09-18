{-# LANGUAGE NoMonomorphismRestriction #-}

{-|

-}
module Main
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Axis
import Diagrams.Swimunit.Grid
import Diagrams.Swimunit.Plot


{-|
  MAIN method. Call 'main' to produce output.
 -}
main :: IO ()
main = mainWith (
    let result = (
            plot
                # centerXY   -- center
            <>
            grid
                # centerXY
            )
    in  result
        <>
        backgroundrect 1.1 result
                # centerXY   -- center background
    )

grid :: Diagram B
grid = (
        verticlabel vml1 0.3 # fc lime
        ||| (
            verticticks vmt1 0.4 ( 0.0) # lc lime
            <>
            verticticks vit1 0.2 (-1.0) # lc green
            <>
            verticaxis 8.5 # lc white
        ) |||
        verticgrid vmt1 8.5  # lc blue
       )

plot :: Diagram B
plot = (
        ploticon xy1 (circle 0.1 # fc blue # lc aqua)
        <>
        plotline xy1    # lc aqua
        )

vmt1 :: [Double]
vmt1 =  [0.0, 2.0, 4.0, 6.0, 8.0]

vit1 :: [Double]
vit1 =  [ 0.5, 1.0, 1.5, 2.5, 3.0, 3.5, 4.5, 5.0, 5.5, 6.5, 7.0, 7.5 ]

vml1 :: [String]
vml1 =  [ "2", "4", "6", "8" ]

xy1 :: [(Double, Double)]
xy1 =  [(1.0,1.0),
        (2.0,3.5),
        (3.0,7.1),
        (3.5,6.9),
        (4.0,2.9),
        (4.5,1.1),
        (5.0,5.0),
        (6.0,5.0),
        (7.0,1.3),
        (7.5,7.0),
        (8.0,8.0)]

backgroundrect :: Double
               -> Diagram B
               -> Diagram B
backgroundrect scl diagram = rect ((width diagram) * scl) ((height diagram) * scl)
                               # lc gray
                               # fc black

 --
----
 --
