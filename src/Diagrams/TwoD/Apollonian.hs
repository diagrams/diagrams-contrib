{-# LANGUAGE Rank2Types 
           , FlexibleContexts
  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Apollonian
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Generation of Apollonian gaskets.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Apollonian 
       ( -- * Circles
         
         Circle(..), mkCircle, center, radius
                                       
         -- * Descartes' Theorem                                       
       , descartes, other, initialConfig
                    
         -- * Apollonian gasket generation
                    
       , apollonian
         
         -- * Diagram generation
         
       , drawCircle
       , drawGasket
       , apollonianGasket
         
       ) where

import Data.Complex
import Data.Foldable (foldMap)

import Diagrams.Prelude hiding (radius, center)

import Control.Arrow (second)
import Data.Colour   (transparent)

------------------------------------------------------------
--  Circles
------------------------------------------------------------

data Circle = Circle { bend :: Double          -- ^ signed bend (reciprocal of radius)
                     , cb   :: Complex Double  -- ^ product of signed bend and center
                     }
  deriving (Eq, Show)

mkCircle :: Double -- ^ signed radius
         -> P2     -- ^ center
         -> Circle
mkCircle r (P (x,y)) = Circle (1/r) (b*x :+ b*y)
  where b = 1/r

center :: Circle -> P2
center (Circle b (cbx :+ cby)) = P (cbx / b, cby / b)

radius :: Circle -> Double
radius = abs . recip . bend

liftF :: (forall a. Floating a => a -> a) -> Circle -> Circle
liftF f (Circle b c) = Circle (f b) (f c)

liftF2 :: (forall a. Floating a => a -> a -> a) -> Circle -> Circle -> Circle
liftF2 f (Circle b1 cb1) (Circle b2 cb2) = Circle (f b1 b2) (f cb1 cb2)

instance Num Circle where
  (+) = liftF2 (+)
  (-) = liftF2 (-)
  (*) = liftF2 (*)
  negate = liftF negate
  abs = liftF abs
  fromInteger n = Circle (fromInteger n) (fromInteger n)
  
instance Fractional Circle where
  (/) = liftF2 (/)
  recip = liftF recip

instance Floating Circle where
  sqrt = liftF sqrt

------------------------------------------------------------
--  Descartes' Theorem
------------------------------------------------------------

--  b1^2 + b2^2 + b3^2 + b4^2 = 1/2(b1 + b2 + b3 + b4)^2.
descartes :: Floating a => [a] -> [a]
descartes [b1,b2,b3] = [r + s, -r + s]
  where r = 2 * sqrt (b1*b2 + b1*b3 + b2*b3)
        s = b1+b2+b3
descartes _ = error "descartes must be called on a list of length 3"

other :: Num a => [a] -> a -> a
other xs x = 2 * sum xs - x

-- | Generate an initial configuration of four mutually tangent
--   circles, given just the signed bends of three of them.
initialConfig :: Double -> Double -> Double -> [Circle]
initialConfig b1 b2 b3 = cs ++ [c4]
  where cs     = [Circle b1 0, Circle b2 ((b2/b1 + 1) :+ 0), Circle b3 cb3]
        a      = 1/b1 + 1/b2
        b      = 1/b1 + 1/b3
        c      = 1/b2 + 1/b3
        x      = (b*b + a*a - c*c)/(2*a)
        y      = sqrt (b*b - x*x)
        cb3    = b3*x :+ b3*y
        [c4,_] = descartes cs

------------------------------------------------------------
--  Gasket generation
------------------------------------------------------------

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x,xs) : (map . second) (x:) (select xs)

apollonian :: [Circle] -> [Circle]
apollonian cs
  =  cs
  ++ (concat . map (\(c,cs') -> apollonian' (other cs' c) cs') . select $ cs)
  
apollonian' :: Circle -> [Circle] -> [Circle]
apollonian' cur others
  | radius cur < 1/500 = []
  | otherwise = cur 
              : (concat $
                   map (\(c, cs') -> apollonian' 
                                       (other (cur:cs') c) 
                                       (cur:cs') 
                       ) 
                       (select others)
                )

------------------------------------------------------------
--  Diagram generation
------------------------------------------------------------

drawCircle :: (Renderable (Path R2) b) => Circle -> Diagram b R2
drawCircle c = circle (radius c) # moveTo (center c)
                                 # lw 0.003 # fcA transparent

drawGasket :: (Renderable (Path R2) b) => [Circle] -> Diagram b R2
drawGasket cs = foldMap drawCircle $ cs

apollonianGasket :: (Renderable (Path R2) b) 
                 => Double -> Double -> Double -> Diagram b R2
apollonianGasket b1 b2 b3 = drawGasket . apollonian $ (initialConfig b1 b2 b3)

