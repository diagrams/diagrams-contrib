{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Example.Logo
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Source code for creating the diagrams logo.
--
-- <<diagrams/src_Diagrams_Example_Logo_diaLogo.svg#diagram=diaLogo&height=100>>
--
-----------------------------------------------------------------------------
module Diagrams.Example.Logo where

-- > import Diagrams.Example.Logo
-- > diaLogo = logo

import           Data.AffineSpace.Point
import           Diagrams.Prelude

import           Diagrams.TwoD.Layout.Tree
import           Diagrams.TwoD.Path.Turtle

import           Control.Lens               ((&), (.~))
import           Control.Monad

------------------------------------------------------------
-- D
------------------------------------------------------------

d = (stroke $
   circle 2 # alignBR # translateX (-0.5)
   <> (hcat' (with & sep.~ 0.2) . map (vcat' (with & sep .~ 0.2))
        $ (replicate 2 (replicate 9 (reversePath $ circle 0.3)))) # alignBR)
    # fc red
    # lw 0

-- an icon-ish version of the d

ico_d = (stroke $
        circle 2 # alignBR # translateX (-0.5)
        <> (vcat' (with & sep.~ 0.3) $ replicate 5 (reversePath $ circle 0.5)) # alignBR)
        # fc red
        # lw 0

------------------------------------------------------------
-- I
------------------------------------------------------------

i = (circle 1 === strutY 0.5 === roundedRect 2 4 0.4)
    # lw 0.05
    # lc blue
    # fc yellow

------------------------------------------------------------
-- A
------------------------------------------------------------

sierpinski 1 = polygon with { polyType = PolyRegular 3 1 }
sierpinski n = t === (t ||| t) # centerX
  where t = sierpinski (n-1)

a1 = sierpinski (4 :: Integer)
     # fc navy
     # lw 0
     # scale (1/2)

------------------------------------------------------------
-- G
------------------------------------------------------------

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' (with & sep.~0.5) $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

gbkg = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withEnvelope (p :: Path R2)
    # lw 0.05
  where p = square 5

g = (text "G" # fontSize 4 # rotateBy (-1/20)) <> gbkg

------------------------------------------------------------
-- R
------------------------------------------------------------

r = sketchTurtle (setHeading 90 >> forward 5 >> right 90
                 >> replicateM 5 (forward 0.9 >> right 36)
                 >> forward 0.9 >> left 135 >> forward 3
                 )
  # reversePath
  # stroke' with { vertexNames = [["end"]] }
  # lw 0.3
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc orange
  # (withName "end" $ atop . place turtle . location)
  where
    turtle = eqTriangle 1 # scaleY 1.3 # rotate (-135 :: Deg)
             # lw 0.1

------------------------------------------------------------
-- A
------------------------------------------------------------

aTree = BNode () f f
  where f = BNode () (leaf ()) (leaf ())

a2 = renderTree (\_ -> circle 0.5 # fc purple) (~~) t'' # lw 0.1
  where Just t' = uniqueXLayout 1 2 aTree
        t''     = forceLayoutTree t'

------------------------------------------------------------
-- M
------------------------------------------------------------

m = square 5 # lw 0.05 <>
    text "m"
      # fontSize 6 # italic # font "freeserif" # fc green

------------------------------------------------------------
-- S
------------------------------------------------------------

vs = map r2 [(5,5), (3,6), (1,5), (1,4), (3,3), (5,2), (4,0), (0,0.5)]
s = (mconcat (map (\v -> translate v (dot blue)) vs) <>
    cubicSpline False (map P vs) # lw 0.20)
    # scale 0.8

dot c = circle 0.4 # fc c # lw 0

------------------------------------------------------------
-- Logo
------------------------------------------------------------

logo = (hcat' (with & sep .~ 0.5) . map alignB $ [ d, i, a1, g, r, a2, m, s ])
       # centerXY
