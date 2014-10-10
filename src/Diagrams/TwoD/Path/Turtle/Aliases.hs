-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Turtle.Aliases
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
--
-- Adds compact aliases for turtle operations, to write code that looks even
-- more Turtle-y.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.Turtle.Aliases where

import           Diagrams.TwoD.Path.Turtle

fd, bk, lt, rt :: (Floating n, Ord n) => n -> Turtle n ()

fd = forward
bk = backward

lt = left
rt = right

pu, pd :: (Floating n, Ord n) => Turtle n ()
pu = penUp
pd = penDown
