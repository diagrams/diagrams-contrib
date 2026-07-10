{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Factorization
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Factorization diagrams, as seen at
-- <http://mathlesstraveled.com/2012/10/05/factorization-diagrams/>
-- and
-- <http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/>
-- and on the cover of Hacker Monthly
-- (<http://hackermonthly.com/issue-31.html>): visually represent the
-- prime factorization of n by drawing n dots recursively grouped
-- according to the factors.
--
-- <<diagrams/src_Diagrams_TwoD_Factorization_grid100Big.svg#diagram=grid100Big&width=600>>
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Factorization where

import           Data.Char        (digitToInt)
import           Data.List.Split  (chunksOf)
import           Data.Maybe       (listToMaybe)
import           Diagrams.Prelude

-- | @primeLayout@ takes a positive integer p (the idea is for it to
--   be prime, though it doesn't really matter) and a diagram, and lays
--   out p rotated copies of the diagram in a circular pattern.
--
--   There is a special case for @p = 2@: if the given diagram is taller
--   than it is wide, then the two copies will be placed beside each
--   other; if wider then tall, they will be placed one above the
--   other.
--
--   The regular @p@-gon connecting the centers of the laid-out
--   diagrams is also filled in with vertical bars of color
--   representing the number @p@.  In particular, there is one color
--   for each decimal digit (the provided list should have length 10
--   and represents the digits 0-9), and the colors, read left to
--   right, give the decimal expansion of @p@.
--
--   > import Diagrams.TwoD.Factorization
--   > plExample
--   >   = pad 1.1 . centerXY
--   >   . hsep 0.5
--   >   . map (sized (mkWidth 1))
--   >   $ [ primeLayout defaultColors 5 (circle 1 # fc black)
--   >     , primeLayout defaultColors 103 (square 1 # fc green # lw none)
--   >     , primeLayout (repeat white) 13 (circle 1 # lc orange)
--   >     ]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_plExample.svg#diagram=plExample&width=400>>
primeLayout :: [Colour Double] -> Integer -> Diagram V2 -> Diagram V2
primeLayout _ 2 d
  | width d >= height d = (d === strutY (height d / 3) === d # reflectY)
                        # centerY
  | otherwise           = (d ||| strutX (width d / 3)  ||| d)
                        # centerX
primeLayout colors p d
  = (mconcat $
       map (\n -> d # translateY r # rotateBy
              (fromIntegral n/fromIntegral p)) [0..p-1]
    )
    <>
    colorBars colors p poly
  where poly = polygon (with & polyType   .~ PolyRegular (fromIntegral p) r
                             & polyOrient .~ OrientH
                            )
        w  = max (width d) (height d)
        r  = w * c / sin (tau / (2 * fromIntegral p))
        c  = 0.75

-- | Draw vertical bars of color inside a polygon which represent the
--   decimal expansion of @p@, using the provided list of colors to
--   represent the digits 0-9.
--
--   > import Diagrams.TwoD.Factorization
--   > colorBarsEx = colorBars defaultColors 3526 (square 1)
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_colorBarsEx.svg#diagram=colorBarsEx&width=200>>
colorBars :: [Colour Double] -> Integer -> Path V2 Double -> Diagram V2
colorBars colors p poly | p <= 11 = stroke poly
                             # fc (colors!!(fromIntegral p `mod` 10))
                             # lw none
colorBars colors p poly = bars # clip poly
  where
    barColors = map ((colors!!) . digitToInt) (show p)
    barW = width poly / fromIntegral (length barColors)
    barH = height poly
    bars = (hcat $ map (\c -> rect barW barH # fc c # lc c) barColors)
           # centerX

-- | A default set of digit colors, based very loosely on the color
--   code for resistors (<http://en.wikipedia.org/wiki/Electronic_color_code>),
--   lightened up a bit by blending with white.
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_showDefaultColors.svg#diagram=showDefaultColors&height=50>>
defaultColors :: [Colour Double]
defaultColors = map (blend 0.1 white)
  [black,red,orange,yellow,green,blue,gray,purple,white,brown]

-- > import Diagrams.TwoD.Factorization
-- > showDefaultColors = hcat $ zipWith showColor defaultColors [0..]
-- >   where
-- >     showColor c d = text (show d) <> square 1 # fc c # lw none

-- | Create a centered factorization diagram from the given list of
--   factors (intended to be primes, but again, any positive integers
--   will do; note how the below example uses 6), by recursively
--   folding according to 'primeLayout', with the 'defaultColors' and
--   a base case of a black circle.
--
--   > import Diagrams.TwoD.Factorization
--   > factorDiagram'Ex = factorDiagram' [2,5,6]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_factorDiagram'Ex.svg#diagram=factorDiagram'Ex&height=200>>
factorDiagram' :: [Integer] -> Diagram V2
factorDiagram' = centerXY . foldr (primeLayout defaultColors) (circle 1 # fc black # lw none)

-- | Create a default factorization diagram for the given integer, by
--   factoring it and calling 'factorDiagram'' on its prime
--   factorization (with the factors ordered from smallest to
--   biggest).
--
--   > import Diagrams.TwoD.Factorization
--   > factorDiagramEx = factorDiagram 700
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_factorDiagramEx.svg#diagram=factorDiagramEx&width=400>>
factorDiagram :: Integer -> Diagram V2
factorDiagram = factorDiagram' . factors

factors :: Integer -> [Integer]
factors 1 = []
factors n = maybe [n] (\a -> a : factors (n `div` a)) mf
  where
    mf = listToMaybe $ filter (\x -> (n `mod` x) == 0) [2 .. n - 1]
    -- only need to go to @intSqrt n@ really

-- | Place a diagram inside a square with the given side length,
--   centering and scaling it to fit with a bit of padding.
--
--   > import Diagrams.TwoD.Factorization
--   > ensquareEx = ensquare 1 (circle 25) ||| ensquare 1 (factorDiagram 30)
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_ensquareEx.svg#diagram=ensquareEx&width=200>>
ensquare
  :: Double -> Diagram V2 -> Diagram V2
ensquare n d = d # centerXY # sized (dims2D (0.8*n) (0.8*n)) <> square n

-- | @fdGrid n@ creates a grid of factorization diagrams, given a list
--   of lists of integers: the inner lists represent L-R rows, which
--   are laid out from top to bottom.
--
--   > import Diagrams.TwoD.Factorization
--   > fdGridEx = fdGrid [[7,6,5],[4,19,200],[1,10,50]]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_fdGridEx.svg#diagram=fdGridEx&width=200>>
fdGrid
  :: [[Integer]] -> Diagram V2
fdGrid  = vcat . map hcat . (map . map) (ensquare 1 . factorDiagram)

-- | @fdGridList n@ creates a grid containing the factorization
--   diagrams of all the numbers from @1@ to @n^2@, ordered left to
--   right, top to bottom (like the grid seen on the cover of Hacker
--   Monthly, <http://hackermonthly.com/issue-31.html>).
--
--   > import Diagrams.TwoD.Factorization
--   > grid100 = fdGridList 10
--   > grid100Big = grid100
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_grid100.svg#diagram=grid100&width=400>>
fdGridList
  :: Integer -> Diagram V2
fdGridList n = fdGrid . chunksOf (fromIntegral n) $ [1..n*n]

-- | @fdTable n@ creates a \"multiplication table\" of factorization
--   diagrams, with the diagrams for @1@ to @n@ along both the top row
--   and left column, and the diagram for @m*n@ in row @m@ and column
--   @n@.
--
--   > import Diagrams.TwoD.Factorization
--   > fdMultTableEx = fdMultTable 13
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_fdMultTableEx.svg#diagram=fdMultTableEx&width=600>>
fdMultTable
  :: Integer -> Diagram V2
fdMultTable n = fdGrid [ [r*c | c <- [1 .. n]] | r <- [1 .. n] ]
