{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Swimunit.Base
where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.Swimunit.Dotmatrix

loglevel :: Int
loglevel = debugll

{-|
  0 Fatal
  1 Error
  2 Warning
  3 Info
  4 Debug
  5 Trace
 -}
debugll :: Int
debugll = 4

errordot :: Diagram B
errordot = circle 1.0

errordotfont :: Dotfont
errordotfont = dotfont_B6x9

{-|
  Constructs a rectangle with an error message passed as argument.
  A medium gray background should offer enough contrast for both light and
  dark themes.
 -}
errord :: String
       -> Diagram B
errord errmsg = (
         dotmatrix errordot errordotfont errmsg
             # lc magenta
             # fc magenta
         <>
         rect 1.0 1.0
             # lc magenta
             # fc gray
         )

{-|
  This defines an empty diagram which is used almost everytime
  when error occurs but the diagram should be rendered anyhow.

  Note that setting this to something visible may help debugging
  diagrams produced by swimunit.
-}
emptyd :: Diagram B
emptyd = circle 0.0

 --
----
 --
