{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.Grid
-- Copyright   :  (c) 2014 Pontus Granström
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  pnutus@gmail.com
--
-- Functions for effortlessly putting lists of diagrams in a grid layout.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Layout.Grid
    (
      gridCat
    , gridCat'
    , gridSnake
    , gridSnake'
    , gridWith

    , sameBoundingRect
    , sameBoundingSquare

    ) where

import           Data.List        (maximumBy)
import           Data.Ord         (comparing)

import           Data.List.Split  (chunksOf)

import           Diagrams.Prelude

-- * Grid Layout

-- | Puts a list of diagrams in a grid, left-to-right, top-to-bottom.
--   The grid is as close to square as possible.
--
-- > import Diagrams.TwoD.Layout.Grid
-- > gridCatExample = gridCat $ map (flip regPoly 1) [3..10]
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Grid_gridCatExample.svg#diagram=gridCatExample&width=200>>

gridCat
  :: TypeableFloat n
  => [QDiagram b V2 n Any]
  -> QDiagram b V2 n Any
gridCat [] = mempty
gridCat diagrams = gridCat' (intSqrt $ length diagrams) diagrams

-- | Same as 'gridCat', but with a specified number of columns.
--
-- > import Diagrams.TwoD.Layout.Grid
-- > gridCatExample' = gridCat' 4 $ map (flip regPoly 1) [3..10]
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Grid_gridCatExample'.svg#diagram=gridCatExample'&width=200>>

gridCat'
  :: TypeableFloat n
  => Int -> [QDiagram b V2 n Any]
  -> QDiagram b V2 n Any
gridCat' = gridAnimal id

-- | Puts a list of diagrams in a grid, alternating left-to-right
--   and right-to-left. Useful for comparing sequences of diagrams.
--   The grid is as close to square as possible.
--
-- > import Diagrams.TwoD.Layout.Grid
-- > gridSnakeExample = gridSnake $ map (flip regPoly 1) [3..10]
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Grid_gridSnakeExample.svg#diagram=gridSnakeExample&width=200>>

gridSnake
  :: TypeableFloat n
  => [QDiagram b V2 n Any]
  -> QDiagram b V2 n Any
gridSnake [] = mempty
gridSnake diagrams = gridSnake' (intSqrt $ length diagrams) diagrams

-- | Same as 'gridSnake', but with a specified number of columns.
--
-- > import Diagrams.TwoD.Layout.Grid
-- > gridSnakeExample' = gridSnake' 4 $ map (flip regPoly 1) [3..10]
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Grid_gridSnakeExample'.svg#diagram=gridSnakeExample'&width=200>>

gridSnake'
  :: TypeableFloat n
  => Int -> [QDiagram b V2 n Any]
  -> QDiagram b V2 n Any
gridSnake' = gridAnimal (everyOther reverse)

-- | Generalisation of gridCat and gridSnake to not repeat code.
gridAnimal
  :: TypeableFloat n
  => ([[QDiagram b V2 n Any]] -> [[QDiagram b V2 n Any]]) -> Int -> [QDiagram b V2 n Any]
  -> QDiagram b V2 n Any
gridAnimal rowFunction cols = vcat . map hcat . rowFunction
    . chunksOf cols . sameBoundingRect . padList cols mempty

-- | `gridWith f (cols, rows)` uses `f`, a function of two
--   zero-indexed integer coordinates, to generate a grid of diagrams
--   with the specified dimensions.
gridWith
  :: TypeableFloat n
  => (Int -> Int -> QDiagram b V2 n Any) -> (Int, Int)
  -> QDiagram b V2 n Any
gridWith f (cols, rows) = gridCat' cols diagrams
  where
    diagrams = [ f x y | y <- [0..rows - 1] , x <- [0..cols - 1] ]

-- * Bounding boxes

-- | Make all diagrams have the same bounding square,
--   one that bounds them all.
sameBoundingSquare
  :: forall b n. TypeableFloat n
  => [QDiagram b V2 n Any]
  -> [QDiagram b V2 n Any]
sameBoundingSquare diagrams = map frameOne diagrams
  where
    biggest        = maximumBy (comparing maxDim) diagrams
    maxDim diagram = max (width diagram) (height diagram)
    centerP        = centerPoint biggest
    padSquare      = (square (maxDim biggest) :: D V2 n) # phantom
    frameOne       = atop padSquare . moveOriginTo centerP


-- | Make all diagrams have the same bounding rect,
--   one that bounds them all.
sameBoundingRect
  :: forall n b. TypeableFloat n
  => [QDiagram b V2 n Any]
  -> [QDiagram b V2 n Any]
sameBoundingRect diagrams = map frameOne diagrams
  where
    widest = maximumBy (comparing width) diagrams
    tallest = maximumBy (comparing height) diagrams
    (xCenter :& _) = coords (centerPoint widest)
    (_ :& yCenter) = coords (centerPoint tallest)
    padRect = (rect (width widest) (height tallest) :: D V2 n) # phantom
    frameOne = atop padRect . moveOriginTo (xCenter ^& yCenter)

-- * Helper functions.

intSqrt :: Int -> Int
intSqrt = round . sqrt . (fromIntegral :: Int -> Float)

everyOther :: (a -> a) -> [a] -> [a]
everyOther f = zipWith ($) (cycle [id, f])

padList :: Int -> a -> [a] -> [a]
padList m padding xs = xs ++ replicate (mod (- length xs) m) padding
