{-# LANGUAGE NoMonomorphismRestriction #-}

{-|

 -}
module Main
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Base

{-|
  MAIN method. Call 'main' to produce output.
 -}
main :: IO ()
main = mainWith (
                error1
                )

error1 :: Diagram B
error1 = errord (
                "[ERROR]{TEST}: "
             ++ "<° "
             ++ "Victor j@gt zw*lf Boxk#mpfer quer "
             ++ "^ber den gro|en Sylter Deich."
             ++ " °>"
             )

 --
----
 --
