{-# LANGUAGE NoMonomorphismRestriction #-}

{-|

 -}
module Main
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Dotmatrix

{-|
  MAIN method. Call 'main' to produce output.
 -}
main :: IO ()
main = mainWith (
           let mname = (
                   dotmatrix    defdot dotfont_B6x9 "SWIMUNIT" # fc aqua  # lc dodgerblue
                   <>
                   notdotmatrix defdot dotfont_B6x9 "SWIMUNIT" # fc black # lc darkslategray
                   ) # centerXY
           in mname
               <>
               backgroundrect 1.1 mname
                   # centerXY   -- center background
       )

defdot :: Diagram B
defdot = circle 1.0

{-|
  Define dark background rectangle.
 -}
backgroundrect :: Double
               -> Diagram B
               -> Diagram B
backgroundrect scl diagram = rect ((width diagram) * scl) ((height diagram) * scl)
                               # lc gray
                               # fc black
 --
----
 --
