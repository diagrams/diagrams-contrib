{-# LANGUAGE Rank2Types 
           , FlexibleContexts
           , ViewPatterns
  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Apollonian
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Generation of Apollonian gaskets.  Any three mutually tangent
-- circles uniquely determine exactly two others which are mutually
-- tangent to all three.  This process can be repeated, generating a
-- fractal circle packing.
--
-- See J. Lagarias, C. Mallows, and A. Wilks, "Beyond the Descartes
-- circle theorem", Amer. Math. Monthly 109 (2002), 338--361.
-- <http://arxiv.org/abs/math/0101066>.
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

------------------------------------------------------------
--  Circles
------------------------------------------------------------

-- | Representation for circles that lets us quickly compute an
--   Apollonian gasket.
data Circle = Circle { bend :: Double   
                       -- ^ The bend is the reciprocal of signed
                       --   radius: a negative radius means the
                       --   outside and inside of the circle are
                       --   switched.  The bends of any four mutually
                       --   tangent circles satisfy Descartes'
                       --   Theorem.
                     , cb   :: Complex Double  
                       -- ^ /Product/ of bend and center represented
                       --   as a complex number.  Amazingly, these
                       --   products also satisfy the equation of
                       --   Descartes' Theorem.
                     }
  deriving (Eq, Show)

-- | Create a @Circle@ given a signed radius and a location for its center.
mkCircle :: Double -- ^ signed radius
         -> P2     -- ^ center
         -> Circle
mkCircle r (unp2 -> (x,y)) = Circle (1/r) (b*x :+ b*y)
  where b = 1/r

-- | Get the center of a circle.
center :: Circle -> P2
center (Circle b (cbx :+ cby)) = p2 (cbx / b, cby / b)

-- | Get the (unsigned) radius of a circle.
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

-- | The @Num@, @Fractional@, and @Floating@ instances for @Circle@
--   (all simply lifted elementwise over @Circle@'s fields) let us use
--   Descartes' Theorem directly on circles.
instance Floating Circle where
  sqrt = liftF sqrt

------------------------------------------------------------
--  Descartes' Theorem
------------------------------------------------------------

-- | Descartes' Theorem states that if @b1@, @b2@, @b3@ and @b4@ are
--   the bends of four mutually tangent circles, then 
--
--   @
--     b1^2 + b2^2 + b3^2 + b4^2 = 1/2 * (b1 + b2 + b3 + b4)^2.
--   @
--
--   Surprisingly, if we replace each of the @bi@ with the /product/
--   of @bi@ and the center of the corresponding circle (represented
--   as a complex number), the equation continues to hold! (See the
--   paper referenced at the top of the module.)
--
--   @descartes [b1,b2,b3]@ solves for @b4@, returning both solutions.
--   Notably, @descartes@ works for any instance of @Floating@, which
--   includes both @Double@ (for bends), @Complex Double@ (for
--   bend/center product), and @Circle@ (for both at once).
descartes :: Floating a => [a] -> [a]
descartes [b1,b2,b3] = [r + s, -r + s]
  where r = 2 * sqrt (b1*b2 + b1*b3 + b2*b3)
        s = b1+b2+b3
descartes _ = error "descartes must be called on a list of length 3"

-- | If we have /four/ mutually tangent circles we can choose one of
--   them to replace; the remaining three determine exactly one other
--   circle which is mutually tangent.  However, in this situation
--   there is no need to apply 'descartes' again, since the two
--   solutions @b4@ and @b4'@ satisfy
--
--   @
--     b4 + b4' = 2 * (b1 + b2 + b3)
--   @
--
--   Hence, to replace @b4@ with its dual, we need only sum the other
--   three, multiply by two, and subtract @b4@.  Again, this works for
--   bends as well as bend/center products.
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

-- | Given a threshold radius and a list of /four/ mutually tangent
--   circles, generate the Apollonian gasket containing those circles.
--   Stop the recursion when encountering a circle with an (unsigned)
--   radius smaller than the threshold.
apollonian :: Double -> [Circle] -> [Circle]
apollonian thresh cs 
  =  cs
  ++ (concat . map (\(c,cs') -> apollonian' thresh (other cs' c) cs') . select $ cs)
  
apollonian' :: Double -> Circle -> [Circle] -> [Circle]
apollonian' thresh cur others
  | radius cur < thresh = []
  | otherwise = cur 
              : (concat $
                   map (\(c, cs') -> apollonian' thresh
                                       (other (cur:cs') c) 
                                       (cur:cs') 
                       ) 
                       (select others)
                )

------------------------------------------------------------
--  Diagram generation
------------------------------------------------------------

-- | Draw a circle.
drawCircle :: (Renderable (Path R2) b) => Double -> Circle -> Diagram b R2
drawCircle w c = circle (radius c) # moveTo (center c)
                                   # lw w # fcA transparent

-- | Draw a generated gasket, using a line width 0.003 times the
--   radius of the largest circle.
drawGasket :: (Renderable (Path R2) b) => [Circle] -> Diagram b R2
drawGasket cs = foldMap (drawCircle w) cs
  where w = (*0.003) . maximum . map radius $ cs

-- | Draw an Apollonian gasket: the first argument is the threshold;
--   the recursion will stop upon reaching circles with radii less than
--   it. The next three arguments are bends of three circles.
apollonianGasket :: (Renderable (Path R2) b) 
                 => Double -> Double -> Double -> Double -> Diagram b R2
apollonianGasket thresh b1 b2 b3 = drawGasket . apollonian thresh $ (initialConfig b1 b2 b3)

