{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}

{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE NoMonomorphismRestriction     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Grid
-- Copyright   :  (c) 2014 Dominic Steinitz
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  dominic@steinitz.org
--
-- <<diagrams/src_Diagrams_TwoD_Grid_example1.svg#diagram=example1&height=300&width=200>>
--
-- The example above is created by the code below which generates a
-- grid, puts points on the interior and the boundary, draws dashed
-- lines around the points to which we wish to draw attention and
-- annotates the points of interest with some text.
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- >
-- > import Diagrams.TwoD.Grid
-- >
-- > example :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>
-- >            Int -> Int -> Diagram b R2
-- > example n m =
-- >
-- >   (gridWithHalves n m) #
-- >
-- >   -- Put points on the boundary
-- >   bndPts [ (1 :: Int,  m + 1     ) | m <- [0,2..2 * m] ] #
-- >   bndPts [ (n + 1,     1 :: Int  ) | n <- [0,2..2 * n] ] #
-- >   bndPts [ (2 * n + 1, m + 1     ) | m <- [0,2..2 * m] ] #
-- >   bndPts [ (n + 1,     2 * m + 1 ) | n <- [0,2..2 * n] ] #
-- >
-- >   intPts [ (n + 1,          m + 1) | n <- [2,4..2 * n - 1] :: [Int]
-- >                                    , m <- [2,4..2 * m - 1] :: [Int] ] #
-- >
-- >   selectedLines (2 * n - 3) (3 :: Int) #
-- >
-- >   ann (2 * n - 1) (1 :: Int) red  #
-- >   ann (2 * n + 1) (3 :: Int) red  #
-- >   ann (2 * n - 1) (3 :: Int) blue #
-- >   ann (2 * n - 3) (3 :: Int) blue #
-- >   ann (2 * n - 1) (5 :: Int) blue
-- >
-- >   where
-- >
-- >     ann n m c = annotate ("u_" ++ show (n `div` 2) ++ show (m `div` 2)) txtPt c n m
-- >
-- >     selectedLines n m = gridLines $ selectedPairs n m
-- >
-- >     selectedPairs n m = let pts = selectedList n m
-- >                         in zip pts (tail pts)
-- >
-- >     selectedList n m = [ (n - 1, m - 1)
-- >                        , (n - 1, m + 1)
-- >                        , (n + 1, m + 1)
-- >                        , (n + 1, m + 3)
-- >                        , (n + 3, m + 3)
-- >                        , (n + 3, m + 1)
-- >                        , (n + 5, m + 1)
-- >                        , (n + 5, m - 1)
-- >                        , (n + 3, m - 1)
-- >                        , (n + 3, m - 3)
-- >                        , (n + 1, m - 3)
-- >                        , (n + 1, m - 1)
-- >                        , (n - 1, m - 1)
-- >                        ]
-- >
-- >     txtPt t = circle cSize # opacity 0.0 # lw 0.0
-- >               ===
-- >               text t # scaleX 0.03 # scaleY 0.03
-- >
-- >     intPts = placeDiagramOnGrid (circle (cSize / 2) # fc blue # opacity 0.5 # lw 0)
-- >     bndPts = placeDiagramOnGrid (circle (cSize / 2) # fc red  # opacity 0.5 # lw 0)
-- >
-- >     cSize :: Double
-- >     cSize = 0.03
-- >
-- > example1 = example 5 5
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Grid (
    gridWithHalves
  , gridWithHalves'
  , annotate
  , gridLine
  , gridLines
  , placeDiagramOnGrid
  ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text

import Control.Lens (makeLenses, (^.))

import Data.Default.Class
import Data.List
import Data.List.Split


data GridOpts
  = GridOpts
    { _gridLineWidth :: Measure R2
    , _gridYColour   :: Colour Double
    , _gridXColour   :: Colour Double
    , _gridLL        :: R2
    , _gridLR        :: R2
    , _gridUL        :: R2
    }

instance Default GridOpts where
  def = GridOpts
        { _gridLineWidth = thin
        , _gridXColour   = red
        , _gridYColour   = blue
        , _gridLL        = r2 (1.0, 1.0)
        , _gridLR        = r2 (2.0, 1.0)
        , _gridUL        = r2 (1.0, 2.0)
        }

data HighlightLineOpts
  = HighlightLineOpts
    { _highLightLineColour        :: Colour Double
    , _highLightLineWidth         :: Measure R2
    , _highLightLineDashingOnOff  :: [Measure R2]
    , _highLightLineDashingOffset :: Measure R2
    }

instance Default HighlightLineOpts where
  def = HighlightLineOpts
        { _highLightLineColour = black
        , _highLightLineWidth = medium
        , _highLightLineDashingOnOff = [Normalized 0.03, Normalized 0.03]
        , _highLightLineDashingOffset = Output 0
        }

makeLenses ''GridOpts
makeLenses ''HighlightLineOpts

-- | Name a point by grid co-ordinates.
tick :: (Renderable Text b, Renderable (Path R2) b) =>
        (Int, Int) -> Diagram b R2
tick (n, m) = pointDiagram origin # named (n, m)

-- | @gridWithHalves@ with default opts.
gridWithHalves :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>
                  Int -> Int -> Diagram b R2
gridWithHalves = gridWithHalves' def

-- | Create a n by m grid. Diagrams can be placed on either the grid
-- points themselves or on points half way between grid points. The
-- latter includes points a half grid length outside of the grid
-- itself.
gridWithHalves' :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>
                   GridOpts -> Int -> Int -> Diagram b R2
gridWithHalves' opts n m =
  (mconcat lineXs # translate (r2 (llx, lly))) <>
  (mconcat lineYs # translate (r2 (llx, lly))) <>
  (intersections # translate (r2 (llx - delta2X, luy + delta2Y)))

  where
    llx :& lly  = coords (opts^.gridLL)
    lrx :& _    = coords (opts^.gridLR)
    _   :& luy  = coords (opts^.gridUL)

    deltaX   = (lrx - llx) / fromIntegral n
    deltaY   = (luy - lly) / fromIntegral m
    delta2X  = (lrx - llx) / fromIntegral (2 * n)
    delta2Y  = (luy - lly) / fromIntegral (2 * m)

    ns  = [0..n]
    ms  = [0..m]
    n2s = [0..2 * n + 2]
    m2s = [0..2 * m + 2]

    xs = map ((* deltaX)  . fromIntegral) ns
    ys = map ((* deltaY)  . fromIntegral) ms

    lineXs = Prelude.map lineX ys
    lineYs = Prelude.map lineY xs

    lineX y = fromOffsets [(opts^.gridLR) ^-^ (opts^.gridLL)] #
              translate (r2 (0.0, y)) #
              lc (opts^.gridXColour) #
              lw (opts^.gridLineWidth)

    lineY x = fromOffsets [(opts^.gridUL) ^-^ (opts^.gridLL)] #
              translate (r2 (x, 0.0)) #
              lc (opts^.gridYColour) #
              lw (opts^.gridLineWidth)

    intersections = hcat $
                    intersperse (strutX delta2X) $
                    map vcat $
                    map (intersperse (strutY delta2Y)) $
                    chunksOf (2 * m + 1 + 2) [ tick (n, m) | n <- n2s, m <- m2s ]

-- | Place a diagram on a grid (which is itself a diagram) at all the
-- co-ordinates specified.
placeDiagramOnGrid :: (IsName n, Renderable Text b, Renderable (Path R2) b) =>
                      Diagram b R2 -> [n] -> Diagram b R2 -> Diagram b R2
placeDiagramOnGrid d = flip $ foldr (\n -> withName n (atop . place d . location))

annotate :: (Renderable Text b, Renderable (Path R2) b) =>
             String ->
             (String -> Diagram b R2) ->
             Colour Double ->
             Int ->
             Int ->
             Diagram b R2 ->
             Diagram b R2
annotate s txtPt h n m =
  withName (n, m) (atop . place (addText s h) . location)
  where
    addText s h = txtPt s # fc h

-- | Draw a line between two named points on the grid.
gridLine :: (IsName a, IsName b, Renderable Text c, Renderable (Path R2) c) =>
            a -> b -> Diagram c R2 -> Diagram c R2
gridLine = gridLine' def

-- | Draw a line between two named points on the grid.
gridLine' :: (IsName a, IsName b, Renderable Text c, Renderable (Path R2) c) =>
            HighlightLineOpts -> a -> b -> Diagram c R2 -> Diagram c R2
gridLine' opts u v =
  withName u $ \x ->
  withName v $ \y ->
  atop ((location x ~~ location y) #
        lc (opts^.highLightLineColour) #
        lw (opts^.highLightLineWidth) #
        dashing (opts^.highLightLineDashingOnOff) (opts^.highLightLineDashingOffset))

-- | Draw lines between a list of pairs of named points on the grid.
gridLines :: (Renderable Text c, Renderable (Path R2) c, IsName a, IsName b) =>
             [(a, b)] -> Diagram c R2 -> Diagram c R2
gridLines xs = foldr (.) id [ gridLine x y | (x, y) <- xs ]
