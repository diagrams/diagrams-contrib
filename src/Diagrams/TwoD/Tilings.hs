-- {-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Tilings
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- Tools for generating and drawing plane tilings made of regular
-- polygons.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Tilings (

  -- * The ring Q[sqrt 2, sqrt 3]

    Q236, rt2, rt3, rt6

  , toFloating

  , Q2, toV2, toP2

  -- * Regular polygons

  , TilingPoly(..)
  , polySides, polyFromSides
  , polyCos, polySin
  , polyRotation, polyExtRotation

  -- * Tilings

  -- ** Types
  , Tiling(..)
  , Edge, mkEdge

  , Polygon(..)

  -- ** Generation

  , TilingState(..), initTilingState
  , TilingM

  , generateTiling

  -- ** Pre-defined tilings

  , t3, t4, t6
  , mk3Tiling, t4612, t488, t31212

  , t3636
  , semiregular
  , rot
  , t3464, t33434, t33344, t33336L, t33336R

  -- * Diagrams

  , drawEdge
  , drawPoly
  , polyColor
  , drawTiling
  , drawTilingStyled

  ) where

import           Control.Monad.State
-- #if __GLASGOW_HASKELL__ >= 704
import           Control.Monad.Writer hiding ((<>))
-- #else
-- import           Control.Monad.Writer
-- #endif

import           Data.Function        (on)
import           Data.List            (mapAccumL, sort)

import qualified Data.Foldable        as F
import qualified Data.Set             as S

import           Data.Colour
import           Diagrams.Prelude

------------------------------------------------------------
-- The ring Q[sqrt(2), sqrt(3)]
------------------------------------------------------------

-- Instead of using Doubles, which can't be compared for equality, it
-- suffices to use elements of the rationals with sqrt(2) and sqrt(3)
-- adjoined.

-- | @Q236 a b c d@ represents @a + b sqrt(2) + c sqrt(3) + d
--   sqrt(6)@.  Note that the @Ord@ instance is suitable for use in
--   `Map` and `Set`, but does not correspond to numeric ordering
--   (@Q236@ is not an ordered field under this ordering).
data Q236 = Q236 Rational Rational Rational Rational
  deriving (Eq, Ord, Show, Read)

-- | Convert a @Q236@ value to a @Double@.
toFloating :: Floating n => Q236 -> n
toFloating (Q236 a b c d) = fromRational a
                        + fromRational b * sqrt 2
                        + fromRational c * sqrt 3
                        + fromRational d * sqrt 6

rt2, rt3, rt6 :: Q236
rt2 = Q236 0 1 0 0
rt3 = Q236 0 0 1 0
rt6 = rt2*rt3

instance Num Q236 where
  (Q236 a1 b1 c1 d1) + (Q236 a2 b2 c2 d2)
      = Q236 (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)
  (Q236 a1 b1 c1 d1) - (Q236 a2 b2 c2 d2)
      = Q236 (a1 - a2) (b1 - b2) (c1 - c2) (d1 - d2)
  (Q236 a1 b1 c1 d1) * (Q236 a2 b2 c2 d2) =
    Q236 (a1*a2 + 2*b1*b2 + 3*c1*c2 + 6*d1*d2)
         (a1*b2 + b1*a2 + 3*c1*d2 + 3*d1*c2)
         (a1*c2 + 2*b1*d2 + c1*a2 + 2*d1*b2)
         (a1*d2 + b1*c2 + c1*b2 + d1*a2)
  abs (Q236 a b c d) = Q236 (abs a) (abs b) (abs c) (abs d)
  fromInteger z = Q236 (fromInteger z) 0 0 0
  signum = error "no signum for Q236"

instance Fractional Q236 where
  recip q@(Q236 a b c d) = Q236 (a3/α) (b3/α) (c3/α) (d3/α)
    where
      q'                 = Q236 a (-b) (-c) d
      rs@(Q236 r 0 0 s)  = q * q'
      rs'                = Q236 r 0 0 (-s)
      (Q236 α 0 0 0)     = rs * rs'
      (Q236 a3 b3 c3 d3) = q' * rs'
  fromRational r = Q236 r 0 0 0

type Q2 = V2 Q236

toV2 :: Floating n => Q2 -> V2 n
toV2 = fmap toFloating

toP2 :: Floating n => Q2 -> P2 n
toP2 = P . toV2

------------------------------------------------------------
-- Polygons
------------------------------------------------------------

-- | Regular polygons which may appear in a tiling of the plane.
data TilingPoly = Triangle | Square | Hexagon | Octagon | Dodecagon
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

polySides :: Num a => TilingPoly -> a
polySides Triangle  = 3
polySides Square    = 4
polySides Hexagon   = 6
polySides Octagon   = 8
polySides Dodecagon = 12

polyFromSides :: (Num a, Eq a, Show a) => a -> TilingPoly
polyFromSides 3  = Triangle
polyFromSides 4  = Square
polyFromSides 6  = Hexagon
polyFromSides 8  = Octagon
polyFromSides 12 = Dodecagon
polyFromSides n  = error $ "Bad polygon number: " ++ show n

-- | Cosine of a polygon's internal angle.
polyCos :: TilingPoly -> Q236
polyCos Triangle  = 1/2
polyCos Square    = 0
polyCos Hexagon   = -1/2
polyCos Octagon   = -1/2
polyCos Dodecagon = -1/2 * rt3

-- | Sine of a polygon's internal angle.
polySin :: TilingPoly -> Q236
polySin Triangle  = (1/2) * rt3
polySin Square    = 1
polySin Hexagon   = (1/2) * rt3
polySin Octagon   = (1/2) * rt2
polySin Dodecagon = 1/2

{-
   R_th = ( cos th  -sin th )
          ( sin th   cos th )

-}

-- | Rotate by polygon internal angle.
polyRotation :: TilingPoly -> Q2 -> Q2
polyRotation p (V2 x y) = V2 (x*c - y*s) (x*s + y*c)
  where c = polyCos p
        s = polySin p

{-
   (cos th  sin th)  ( -1  0 )  =  (-cos th  -sin th)
   (-sin th  cos th) ( 0  -1 )     (sin th   -cos th)
-}

-- | Rotate by polygon external angle.
polyExtRotation :: TilingPoly -> Q2 -> Q2
polyExtRotation p (V2 x y) = V2 (-x*c - y*s) (x*s - y*c)
  where c = polyCos p
        s = polySin p

------------------------------------------------------------
-- Tilings
------------------------------------------------------------

-- | A tiling, represented as a sort of zipper. @curConfig@ indicates
--   the polygons around the current vertex, in couterclockwise order
--   starting from the edge along which we entered the vertex.
--   @follow@ allows one to move along an edge to an adjacent vertex,
--   where the edges are numbered counterclockwise from zero,
--   beginning with the edge along which we entered the current
--   vertex.
data Tiling = Tiling { curConfig :: [TilingPoly]
                     , follow    :: Int -> Tiling
                     }

-- | An edge is represented by a pair of vertices.  Do not use the
--   @Edge@ constructor directly; use 'mkEdge' instead.
data Edge = Edge Q2 Q2
  deriving (Eq, Ord, Show)

-- | Smart constructor for @Edge@, which puts the vertices in a
--   canonical order.
mkEdge :: Q2 -> Q2 -> Edge
mkEdge v1 v2 | v1 <= v2  = Edge v1 v2
             | otherwise = Edge v2 v1

-- | A polygon is represented by a list of its vertices, in
--   counterclockwise order.  However, the @Eq@ and @Ord@ instances
--   for polygons ignore the order.
newtype Polygon = Polygon { polygonVertices :: [Q2] }
  deriving Show

instance Eq Polygon where
  (Polygon vs1) == (Polygon vs2) = sort vs1 == sort vs2

instance Ord Polygon where
  compare = compare `on` (sort . polygonVertices)

-- | The state maintained while generating a tiling, recording which
--   vertices have been visited and which edges and polygons have been
--   drawn.
data TilingState = TP { visitedVertices :: S.Set Q2
                      , visitedEdges    :: S.Set Edge
                      , visitedPolygons :: S.Set Polygon
                      }

initTilingState :: TilingState
initTilingState = TP S.empty S.empty S.empty

-- | The @TilingM@ monad tracks a @TilingState@, and can output
--   elements of some monoid @w@ along the way.
type TilingM w a = WriterT w (State TilingState) a

generateTiling :: forall w. Monoid w
               => Tiling        -- ^ The tiling to generate
               -> Q2            -- ^ The location of the starting vertex.
               -> Q2            -- ^ The starting direction, i.e. the
                                --   direction along which we came into
                                --   the starting vertex.
               -> (Q2 -> Bool)  -- ^ Predicate on vertices specifying
                                --   which should be visited.  The
                                --   vertices for which the predicate
                                --   evaluates to True must form a
                                --   single connected component.
               -> (Edge -> w)          -- ^ what to do with edges
               -> (Polygon -> w)       -- ^ what to do with polygons
               -> w
generateTiling t v d vPred e p
  = evalState (execWriterT (generateTiling' t v d)) initTilingState where

  generateTiling' :: Tiling -> Q2 -> Q2 -> TilingM w ()
  generateTiling' t v d
      -- stop if the current vertex fails the predicate
    | not (vPred v) = return ()
    | otherwise = do
        ts <- get

        -- stop if we've seen this vertex before
        when (v `S.notMember` visitedVertices ts) $ do

          -- otherwise, mark it as visited
          modify (\ts -> ts { visitedVertices = v `S.insert` visitedVertices ts })

          -- get the neighboring vertices and the polygons surrounding
          -- this vertex, and filter out ones we've already generated
          let (neighbors, polys) = genNeighbors t v d
              edges  = S.fromList $ map (mkEdge v) neighbors
              edges' = edges `S.difference` visitedEdges ts
              polys' = polys `S.difference` visitedPolygons ts

          -- generate some edges and polygons
          F.mapM_ (tell . e) edges'
          F.mapM_ (tell . p) polys'

          -- remember that we generated them
          modify (\ts -> ts { visitedEdges = edges' `S.union` visitedEdges ts })
          modify (\ts -> ts { visitedPolygons = polys' `S.union` visitedPolygons ts })

          -- follow edges and continue recursively
          zipWithM_ (\d i -> generateTiling' (follow t i) (v ^+^ d) d)
            (map (^-^ v) neighbors) [0..]

-- | Generate the neighboring vertices and polygons of a given vertex.
genNeighbors :: Tiling -> Q2 -> Q2 -> ([Q2], S.Set Polygon)
genNeighbors t v d = (neighbors, S.fromList polys) where
  (neighbors, polys)
    = unzip . snd
      $ mapAccumL
          (\d' poly -> (polyRotation poly d', (v ^+^ d', genPolyVs poly v d')))
          (negated d)
          (curConfig t)

-- | Generate the vertices of the given polygon, with one vertex at the given point
--   and an adjacent vertex at the given offset.
genPolyVs :: TilingPoly
          -> Q2          -- ^ one vertex
          -> Q2          -- ^ vector to second vertex
          -> Polygon
genPolyVs p v d = Polygon
                . scanl (^+^) v
                . take (polySides p - 1)
                . iterate (polyExtRotation p)
                $ d

------------------------------------------------------------
-- Diagrams
------------------------------------------------------------

-- | Draw an edge with the given style.
drawEdge :: Style V2 Double-> Edge -> Diagram V2
drawEdge s (Edge v1 v2) = (toP2 v1 ~~ toP2 v2) # applyStyle s

-- | Draw a polygon with the given style.
drawPoly :: (Polygon -> Style V2 Double) -> Polygon -> Diagram V2
drawPoly s p = applyStyle (s p) . stroke . mapLoc closeLine . fromVertices . map toP2 . polygonVertices $ p

-- Simple per-polygon color scheme
polyColor :: (Floating a, Ord a) => TilingPoly -> Colour a
polyColor Triangle  = yellow
polyColor Square    = mediumseagreen
polyColor Hexagon   = blueviolet
polyColor Octagon   = lightsteelblue
polyColor Dodecagon = cornflowerblue

-- | Draw a tiling, with a given width and height and default colors
--   for the polygons.
drawTiling :: Tiling -> Double -> Double -> Diagram V2
drawTiling =
  drawTilingStyled
    mempty
    (\p -> mempty
           # lw none
           # fc ( polyColor
                . polyFromSides
                . length
                . polygonVertices
                $ p
                )
    )

-- | Draw a tiling with customizable styles for the polygons.  This is
--   just an example, which you can use as the basis of your own
--   tiling-drawing routine.
drawTilingStyled :: Style V2 Double -> (Polygon -> Style V2 Double)
                 -> Tiling -> Double -> Double -> Diagram V2
drawTilingStyled eStyle pStyle t w h =
  mkDia $ generateTiling t (V2 0 0) (V2 1 0) inRect

            -- draw the edges and polygons into separate
            -- diagrams, so we can make sure all the edges are
            -- overlaid on top of all the polygons at the end
            (liftA2 (,) (drawEdge eStyle) mempty)
            (liftA2 (,) mempty (drawPoly pStyle))
  where
    inRect (toV2 -> V2 x y) = -w/2 <= x && x <= w/2 && -h/2 <= y && y <= h/2
    mkDia (es, ps) = viewRect (es <> ps)
    viewRect = withEnvelope (rect w h :: Path V2 Double)

------------------------------------------------------------
-- Some pre-defined tilings
------------------------------------------------------------

-- Regular tilings

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t3D.svg#diagram=t3D&width=300>>
t3 :: Tiling
t3 = Tiling (replicate 6 Triangle) (const t3)

-- > import Diagrams.TwoD.Tilings
-- > t3D = drawTiling t3 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t4D.svg#diagram=t4D&width=300>>
t4 :: Tiling
t4 = Tiling (replicate 4 Square) (const t4)

-- > import Diagrams.TwoD.Tilings
-- > t4D = drawTiling t4 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t6D.svg#diagram=t6D&width=300>>
t6 :: Tiling
t6 = Tiling (replicate 3 Hexagon) (const t6)

-- > import Diagrams.TwoD.Tilings
-- > t6D = drawTiling t6 10 10

-- Semi-regular tilings

-- | Create a tiling with the same 3 polygons surrounding each vertex.
--   The argument is the number of sides of the polygons surrounding a vertex.
mk3Tiling :: [Int] -> Tiling
mk3Tiling (ps@[a,b,c])
      = Tiling
          (map polyFromSides ps)
          (\i -> case i `mod` 3 of
                   0 -> mk3Tiling (reverse ps)
                   1 -> mk3Tiling [a,c,b]
                   2 -> mk3Tiling [b,a,c]
                   _ -> error "i `mod` 3 is not 0, 1,or 2! the sky is falling!"
          )
mk3Tiling _ = error "mk3Tiling may only be called on a list of length 3."

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t4612D.svg#diagram=t4612D&width=300>>
t4612 :: Tiling
t4612 = mk3Tiling [4,6,12]

-- > import Diagrams.TwoD.Tilings
-- > t4612D = drawTiling t4612 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t488D.svg#diagram=t488D&width=300>>
t488 :: Tiling
t488 = mk3Tiling [4,8,8]

-- > import Diagrams.TwoD.Tilings
-- > t488D = drawTiling t488 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t31212D.svg#diagram=t31212D&width=300>>
t31212 :: Tiling
t31212 = mk3Tiling [3,12,12]

-- > import Diagrams.TwoD.Tilings
-- > t31212D = drawTiling t31212 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t3636D.svg#diagram=t3636D&width=300>>
t3636 :: Tiling
t3636 = mkT [3,6,3,6]
  where mkT :: [Int] -> Tiling
        mkT ps = Tiling (map polyFromSides ps)
                        (\i -> mkT $ if even i then reverse ps else ps)

-- > import Diagrams.TwoD.Tilings
-- > t3636D = drawTiling t3636 10 10

-- | Create a tiling where every vertex is the same up to rotation and
--   translation (but /not/ reflection).  Arbitrarily pick one of the
--   edges emanating from a vertex and number the edges
--   counterclockwise starting with 0 for the chosen edge.
semiregular :: [Int]   -- ^ The number of sides of the polygons
                       --   surrounding a typical vertex,
                       --   counterclockwise starting from edge 0.
            -> [Int]   -- ^ The transition list: if the /i/th entry of
                       --   this list is /j/, it indicates that the edge
                       --   labeled /i/ is labeled /j/ with respect to
                       --   the vertex on its other end.
            -> Tiling
semiregular ps trans = mkT 0
  where mkT i = Tiling
                  (map polyFromSides (rot i ps))
                  (\j -> mkT $ rot i trans !! j)

rot :: (Num a, Eq a) => a -> [t] -> [t]
rot 0 xs     = xs
rot _ []     = []
rot n (x:xs) = rot (n-1) (xs ++ [x])

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t3464D.svg#diagram=t3464D&width=300>>
t3464 :: Tiling
t3464 = semiregular [4,3,4,6] [3,2,1,0]

-- > import Diagrams.TwoD.Tilings
-- > t3464D = drawTiling t3464 10 10

{-

The above is worth a few lines of explanation.  There is only one type
of vertex, of degree 4, hence there are four possible states depending
on which edge one entered the vertex on.  We can arbitrarily choose
state 0 to be the one in which the surrounding polygons, ccw from the
edge on which the vertex was entered, are 4,3,4,6.  The second list
then records the states in which one ends up after following edges 0,
1, 2... (numbered ccw with edge 0 being the one entered on) starting
from state 0.  The transitions from other states can be worked out by
appropriate cyclic shifts.

The tilings below are worked out in a similar manner.

-}

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t33434D.svg#diagram=t33434D&width=300>>
t33434 :: Tiling
t33434  = semiregular [3,4,3,4,3] [0,2,1,4,3]

-- > import Diagrams.TwoD.Tilings
-- > t33434D = drawTiling t33434 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t33344D.svg#diagram=t33344D&width=300>>
t33344 :: Tiling
t33344  = semiregular [4,3,3,3,4] [0,4,2,3,1]

-- > import Diagrams.TwoD.Tilings
-- > t33344D = drawTiling t33344 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t33336LD.svg#diagram=t33336LD&width=300>>
t33336L :: Tiling
t33336L = semiregular [3,3,3,3,6] [4,1,3,2,0]

-- > import Diagrams.TwoD.Tilings
-- > t33336LD = drawTiling t33336L 10 10

-- | <<diagrams/src_Diagrams_TwoD_Tilings_t33336RD.svg#diagram=t33336RD&width=300>>
t33336R :: Tiling
t33336R = semiregular [3,3,3,3,6] [4,2,1,3,0]

-- > import Diagrams.TwoD.Tilings
-- > t33336RD = drawTiling t33336R 10 10
