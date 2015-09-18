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
       let alphabet = (
                withoutDarkDots  -- print without panel
                ===
                (
                withoutDarkDots  -- print without panel
                <>
                darkDots         -- print panel
                )
                )   # centerXY
       in alphabet
          <>
          backgroundrect 1.1 alphabet
              # centerXY   -- center background
        )

{-|
  Render testing alphabet dotmatrix.
 -}
withoutDarkDots :: Diagram B
withoutDarkDots = (
                dotmatrix defdot dotfont_B6x9 line1 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line2 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line3 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line4 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line5 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line6 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 line7 # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 "" # fc blue # lc aqua
                ===
                dotmatrix defdot dotfont_B6x9 "" # fc blue # lc aqua
    )

{-|
  Render background panel of testing alphabet.
 -}
darkDots :: Diagram B
darkDots = (
                notdotmatrix defdot dotfont_B6x9 line1 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line2 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line3 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line4 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line5 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line6 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 line7 # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 "" # fc black # lc darkslategray
                ===
                notdotmatrix defdot dotfont_B6x9 "" # fc black # lc darkslategray
    )

{-|
  Define testing alphabet.
 -}
line1 :: String
line1 = "ABCDEFGHIJ"    -- fully implemented

line2 :: String
line2 = "KLMNOPQRST"    -- fully implemented

line3 :: String
line3 = "UVWXYZ ÄÖÜ"    -- fully implemented

line4 :: String
line4 = "1234567890"    -- fully implemented

line5 :: String
line5 = "#:,;.+-_!="    -- fully implemented

line6 :: String
line6 = "&*{}[]'<|>"

line7 :: String
line7 = "^°~@$%/()?"

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
