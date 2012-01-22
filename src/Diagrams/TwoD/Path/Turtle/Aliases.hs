-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Turtle.Aliases
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Adds compact aliases for turtle operations, to write code that looks even
-- more Turtle-y.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.Turtle.Aliases where

import Diagrams.TwoD.Path.Turtle

fd, bk, lt, rt :: Double -> Turtle ()

fd = forward
bk = backward

lt = left
rt = right

pu, pd :: Turtle ()
pu = penUp
pd = penDown
