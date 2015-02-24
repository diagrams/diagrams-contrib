{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Diagrams.Prelude

import           Diagrams.TwoD.Layout.Tree
import           Diagrams.TwoD.Path.Turtle

import           Control.Monad

------------------------------------------------------------
-- D
------------------------------------------------------------

d = (stroke $
   circle 2 # alignBR # translateX (-0.5)
   <> (hcat' (with & sep.~ 0.2) . map (vcat' (with & sep .~ 0.2))
        $ (replicate 2 (replicate 9 (reversePath $ circle 0.3)))) # alignBR)
    # fc red
    # lwG 0

-- an icon-ish version of the d

ico_d = (stroke $
        circle 2 # alignBR # translateX (-0.5)
        <> (vcat' (with & sep.~ 0.3) $ replicate 5 (reversePath $ circle 0.5)) # alignBR)
        # fc red
        # lwG 0

------------------------------------------------------------
-- I
------------------------------------------------------------

i = (circle 1 === strutY 0.5 === roundedRect 2 4 0.4)
    # lwG 0.05
    # lc blue
    # fc yellow

------------------------------------------------------------
-- A
------------------------------------------------------------

sierpinski 1 = polygon (with & polyType .~ PolyRegular 3 1 )
sierpinski n = t === (t ||| t) # centerX
  where t = sierpinski (n-1)

a1 = sierpinski (4 :: Integer)
     # fc navy
     # lwG 0
     # scale (1/2)

------------------------------------------------------------
-- G
------------------------------------------------------------

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' (with & sep.~0.5) $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

gbkg :: forall b n m. (TrailLike (QDiagram b V2 n m), Monoid m, Semigroup m,
                       TypeableFloat n) =>
        QDiagram b V2 n m
gbkg = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withEnvelope (p :: Path V2 n)
    # lwG 0.05
  where p = square 5

g = (text "G" # fontSizeG 4 # rotateBy (-1/20)) <> gbkg

------------------------------------------------------------
-- R
------------------------------------------------------------

r = sketchTurtle (setHeading 90 >> forward 5 >> right 90
                 >> replicateM 5 (forward 0.9 >> right 36)
                 >> forward 0.9 >> left 135 >> forward 3
                 )
  # reversePath
  # stroke' (with & vertexNames .~ [["end"]] )
  # lwG 0.3
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc orange
  # (withName "end" $ atop . place turtle . location)
  where
    turtle = eqTriangle 1 # scaleY 1.3 # rotate (-135 @@ deg)
             # lwG 0.1

------------------------------------------------------------
-- A
------------------------------------------------------------

aTree = BNode () f f
  where f = BNode () (leaf ()) (leaf ())

a2 = renderTree (\_ -> circle 0.5 # fc purple) (~~) t'' # lwG 0.1
  where Just t' = uniqueXLayout 1 2 aTree
        t''     = forceLayoutTree t'

------------------------------------------------------------
-- M
------------------------------------------------------------

m = square 5 # lwG 0.05 <>
    text "m"
      # fontSizeG 6 # italic # font "freeserif" # fc green

------------------------------------------------------------
-- S
------------------------------------------------------------

ps = map p2 [(5,5), (3,6), (1,5), (1,4), (3,3), (5,2), (4,0), (0,0.5)]
s = (mconcat (map (place (disk blue)) ps) <>
    cubicSpline False ps # lwG 0.20)
    # scale 0.8

disk c = circle 0.4 # fc c # lwG 0

------------------------------------------------------------
-- Logo
------------------------------------------------------------

logo = (hcat' (with & sep .~ 0.5) . map alignB $ [ d, i, a1, g, r, a2, m, s ])
       # centerXY
