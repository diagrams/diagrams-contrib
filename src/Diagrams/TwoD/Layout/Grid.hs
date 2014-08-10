{-# LANGUAGE FlexibleContexts #-} 

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

import Data.List       (maximumBy)
import Data.Ord        (comparing)

import Data.List.Split (chunksOf)
    
import Diagrams.Prelude

-- * Grid Layout

-- | Puts a list of diagrams in a grid, left-to-right, top-to-bottom.
--   The grid is as close to square as possible.
gridCat
  :: (Backend b R2, Renderable (Path R2) b)
  => [Diagram b R2] 
  -> Diagram b R2
gridCat diagrams = gridCat' (intSqrt $ length diagrams) diagrams

-- | Same as 'gridCat', but with a specified number of columns.
gridCat' 
  :: (Backend b R2, Renderable (Path R2) b)
  => Int -> [Diagram b R2] 
  -> Diagram b R2
gridCat' = gridAnimal id

-- | Puts a list of diagrams in a grid, alternating left-to-right
--   and right-to-left. Useful for comparing sequences of diagrams.
--   The grid is as close to square as possible.
gridSnake
  :: (Backend b R2, Renderable (Path R2) b)
  => [Diagram b R2] 
  -> Diagram b R2
gridSnake diagrams = gridSnake' (intSqrt $ length diagrams) diagrams

-- | Same as 'gridSnake', but with a specified number of columns.
gridSnake'
  :: (Backend b R2, Renderable (Path R2) b)
  => Int -> [Diagram b R2] 
  -> Diagram b R2
gridSnake' = gridAnimal (everyOther reverse)

-- | Generalisation of gridCat and gridSnake to not repeat code.
gridAnimal 
  :: (Backend b R2, Renderable (Path R2) b)
  => ([[Diagram b R2]] -> [[Diagram b R2]]) -> Int -> [Diagram b R2] 
  -> Diagram b R2
gridAnimal rowFunction cols = vcat . map hcat . rowFunction 
    . chunksOf cols . sameBoundingRect . padList cols mempty

-- | `gridWith f (cols, rows)` uses `f`, a function of two 
--   zero-indexed integer coordinates, to generate a grid of diagrams
--   with the specified dimensions.
gridWith 
  :: (Backend b R2, Renderable (Path R2) b)
  => (Int -> Int -> Diagram b R2) -> (Int, Int) 
  -> Diagram b R2
gridWith f (cols, rows) = gridCat' cols diagrams
  where
    diagrams = [ f x y | y <- [0..rows - 1] , x <- [0..cols - 1] ]

-- * Bounding boxes

-- | Make all diagrams have the same bounding square,
--   one that bounds them all.
sameBoundingSquare
  :: (Backend b R2, Renderable (Path R2) b)
  => [Diagram b R2]
  -> [Diagram b R2]
sameBoundingSquare diagrams = map frameOne diagrams
  where
    biggest = maximumBy (comparing size) diagrams
    size diagram = max (width diagram) (height diagram)
    centerPoint = center2D biggest
    padSquare = (square (size biggest) :: D R2) # phantom
    frameOne = atop padSquare . moveOriginTo centerPoint


-- | Make all diagrams have the same bounding rect,
--   one that bounds them all.
sameBoundingRect
  :: (Backend b R2, Renderable (Path R2) b)
  => [Diagram b R2]
  -> [Diagram b R2]
sameBoundingRect diagrams = map frameOne diagrams
  where
    widest = maximumBy (comparing width) diagrams
    tallest = maximumBy (comparing height) diagrams
    (xCenter :& _) = coords (center2D widest)
    (_ :& yCenter) = coords (center2D tallest)
    padRect = (rect (width widest) (height tallest) :: D R2) # phantom
    frameOne = atop padRect . moveOriginTo (xCenter ^& yCenter)

-- * Helper functions.

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

everyOther :: (a -> a) -> ([a] -> [a])
everyOther f xs = zipWith ($) (cycle [id, f]) xs

padList :: Int -> a -> [a] -> [a]
padList m pad xs = xs ++ replicate (mod (- length xs) m) pad