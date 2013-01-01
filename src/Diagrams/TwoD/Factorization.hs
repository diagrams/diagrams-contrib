{-# LANGUAGE FlexibleContexts #-}
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
-----------------------------------------------------------------------------

module Diagrams.TwoD.Factorization where

import Diagrams.Prelude
import Data.List.Split  ( chunksOf   )
import Data.Char        ( digitToInt )

import Math.NumberTheory.Primes.Factorisation (factorise)

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
primeLayout :: (Backend b R2, Renderable (Path R2) b)
            => [Colour Double] -> Integer -> Diagram b R2 -> Diagram b R2
primeLayout _ 2 d
  | width d >= height d = (d === strutY (height d / 3) === d # reflectY)
                        # centerY
  | otherwise           = (d ||| strutX (width d / 3)  ||| d)
                        # centerX
primeLayout colors p d
  = (mconcat $
       iterateN (fromIntegral p)
         (rotateBy (1/fromIntegral p))
         (d # translateY r)
    )
    <>
    colorBars colors p poly
  where poly = polygon with { polyType   = PolyRegular (fromIntegral p) r
                            , polyOrient = OrientH
                            }
        w  = max (width d) (height d)
        r  = w * c / sin (tau / (2 * fromIntegral p))
        c  = 0.75

-- | Draw vertical bars of color inside a @p@-gon representing the
--   decimal expansion of @p@, using the provided list of colors to
--   represent the digits 0-9.
colorBars :: (Renderable (Path R2) b)
          => [Colour Double] -> Integer -> Path R2 -> Diagram b R2
colorBars colors p poly | p <= 11 = stroke poly
                             # fc (colors!!(fromIntegral p `mod` 10))
                             # lw 0
colorBars colors p poly = bars # clipBy poly
  where
    barColors = map ((colors!!) . digitToInt) (show p)
    barW = width poly / fromIntegral (length barColors)
    barH = height poly
    bars = (hcat $ map (\c -> rect barW barH # fc c # lc c) barColors)
           # centerX

-- | A default set of digit colors, based very loosely on the color
--   code for resistors (<http://en.wikipedia.org/wiki/Electronic_color_code>),
--   lightened up a bit by blending with white.
defaultColors :: [Colour Double]
defaultColors = map (blend 0.1 white)
  [black,red,orange,yellow,green,blue,gray,purple,white,brown]

-- | Create a centered factorization diagram from the given list of
--   factors (intended to be primes, but again, any positive integers
--   will do), by recursively folding according to 'primeLayout', with
--   the 'defaultColors' and a base case of a black circle.
factorDiagram' :: (Backend b R2, Renderable (Path R2) b)
               => [Integer] -> Diagram b R2
factorDiagram' = centerXY . foldr (primeLayout defaultColors) (circle 1 # fc black)


-- | Create a default factorization diagram for the given integer, by
--   factoring it and calling 'factorDiagram'' on its prime
--   factorization (with the factors ordered from smallest to
--   biggest).
factorDiagram :: (Backend b R2, Renderable (Path R2) b)
              => Integer -> Diagram b R2
factorDiagram = factorDiagram'
              . concatMap (uncurry $ flip replicate)
              . factorise

-- | Place a diagram inside a square with the given side length,
--   centering and scaling it to fit with a bit of padding.
ensquare
  :: (Backend b R2, Renderable (Path R2) b)
  => Double -> Diagram b R2 -> Diagram b R2
ensquare n d = d # centerXY # sized (Dims (0.8*n) (0.8*n)) <> square n

-- | @fdGrid n@ creates a grid of factorization diagrams, given a list
--   of lists of integers: the inner lists represent L-R rows, which
--   are laid out from top to bottom.
fdGrid
  :: (Renderable (Path R2) b, Backend b R2)
  => [[Integer]] -> Diagram b R2
fdGrid  = vcat . map hcat . (map . map) (ensquare 1 . factorDiagram)

-- | @fdGridList n@ creates a grid containing the factorization
--   diagrams of all the numbers from @1@ to @n^2@, ordered left to
--   right, top to bottom (like the grid seen on
fdGridList
  :: (Renderable (Path R2) b, Backend b R2)
  => Integer -> Diagram b R2
fdGridList n = fdGrid . chunksOf (fromIntegral n) $ [1..n*n]

-- | @fdTable n@ creates a \"multiplication table\" of factorization
--   diagrams, with the diagrams for @1@ to @n@ along both the top row
--   and left column, and the diagram for @m*n@ in row @m@ and column
--   @n@.
fdMultTable
  :: (Renderable (Path R2) b, Backend b R2)
  => Integer -> Diagram b R2
fdMultTable n = fdGrid $ [ [r*c | c <- [1 .. n]] | r <- [1 .. n] ]
