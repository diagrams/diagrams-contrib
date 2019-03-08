{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.IntersectionExtras
-- Copyright   :  (c) 2018 Mike Zuser
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Mike Zuser <mikezuser@gmail.com>
--
-- Extra functions for working with the intersections of `Path`s. This
-- module was motivated by `explodeIntersections`. The rest of the module is
-- either functions that where needed to build it or functions to help
-- consume it.
-----------------------------------------------------------------------------
module Diagrams.TwoD.Path.IntersectionExtras
  ( -- * Intersection Parameters
    intersectParams, intersectParams'
  , intersectParamsP, intersectParamsP'
  , intersectParamsT, intersectParamsT'
    -- * Splitting Segments
  , splitSeg
  , sliceSeg
    -- * Rad Explosions
  , explodeIntersections, explodeIntersections'
    -- * Consuming Exploded Paths
  , onExplodedPath
  , onExplodedIntersections, onExplodedIntersections'
  )where
import Data.List

import Diagrams.Prelude
import Diagrams.TwoD.Segment

-- defEps uses the value from Diagrams.TwoD.Path
defEps :: Fractional n => n
defEps = 1e-8

-----------------------------------------------------------------------------
-- Intersection Parameters --------------------------------------------------
-----------------------------------------------------------------------------

-- | Find the intersect parameters of two objects that can be converted to a
--   path. A parameter a value in [0,1] which is the proportion of a segment
--   that is the start of the segment and the intersection point. If your
--   object is made of multiple segments, you will need to correlate the
--   parameter to the segment yourself.
intersectParams :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) => t -> s -> [(n, n)]
intersectParams = intersectParams' defEps

-- | Find the intersect parameters of two objects that can be converted to a
--   path within the given tolerance.
intersectParams' :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) => n -> t -> s -> [(n, n)]
intersectParams' eps as bs = intersectParamsP' eps (toPath as) (toPath bs)

-- | Compute the intersect parameters between two paths.
intersectParamsP :: OrderedField n => Path V2 n -> Path V2 n -> [(n, n)]
intersectParamsP = intersectParamsP' defEps

-- | Compute the intersect parameters between two paths within the given tolerance.
intersectParamsP' :: OrderedField n => n -> Path V2 n -> Path V2 n -> [(n, n)]
intersectParamsP' eps as bs = do
  a <- pathTrails as
  b <- pathTrails bs
  intersectParamsT' eps a b

-- | Compute the intersect parameters between two located trails.
intersectParamsT :: OrderedField n => Located (Trail V2 n) -> Located (Trail V2 n) -> [(n, n)]
intersectParamsT = intersectParamsT' defEps

-- | Compute the intersect parameters between two located trails within the
--   given tolerance.
intersectParamsT' :: OrderedField n => n -> Located (Trail V2 n) -> Located (Trail V2 n) -> [(n, n)]
intersectParamsT' eps as bs = do
  a <- fixTrail as
  b <- fixTrail bs
  map (\(p, q, _) -> (p, q)) $ segmentSegment eps a b

-----------------------------------------------------------------------------
-- Splitting Segments -------------------------------------------------------
-----------------------------------------------------------------------------

-- | Spit a segment at a given parameter,
splitSeg :: (Additive v, Num n) => FixedSegment v n -> n -> (FixedSegment v n, FixedSegment v n)
splitSeg (FLinear p0 p1) t = (FLinear p0 sp, FLinear sp p1)
  where
    interp = flip $ lerp t
    sp = interp p0 p1
-- Based on Geom2D.CubicBezier.Basic.splitBezierCubic
splitSeg (FCubic p0 p1 p2 p3) t = (FCubic p0 p01 p0112 sp, FCubic sp p1223 p23 p3)
  where
    interp = flip $ lerp t
    p12   = interp p1    p2
    p01   = interp p0    p1
    p0112 = interp p01   p12
    sp    = interp p0112 p1223
    p1223 = interp p12   p23
    p23   = interp p2    p3

-- | Get the slice of a segment between two parameters.
sliceSeg :: (Additive v, OrderedField n) => FixedSegment v n -> n -> n -> FixedSegment v n
-- Based on Geom2D.CubicBezier.Basic.bezierSubsegment
sliceSeg s t1 t2
  | t1 > t2   = sliceSeg s t2 t1
  | t2 == 0   = fst $ splitSeg s t1
  | otherwise = snd $ flip splitSeg (t1/t2) $ fst $ splitSeg s t2

-----------------------------------------------------------------------------
-- Rad Explosions -----------------------------------------------------------
-----------------------------------------------------------------------------

-- | Turn a path into separate trails such that no trail intersects with any
--   other. `explodePath` where additionally each trail is split at all it's
--   intersections.
explodeIntersections :: (InSpace V2 n t, TrailLike t) => Path V2 n -> [[[t]]]
explodeIntersections = explodeIntersections' defEps

-- | `explodeIntersections` with intersections calculated within the given
--   tolerance.
explodeIntersections' :: (InSpace V2 n t, TrailLike t) => n -> Path V2 n -> [[[t]]]
explodeIntersections' eps path = map (map $ map toTrailLike . cut) explodedPath
  where
    explodedPath = explodePath path
    toTrailLike  = fromLocSegments . mapLoc (:[]) . fromFixedSeg
    toFixedSeg   = mkFixedSeg . mapLoc (head . trailSegments)
    cut t = foldr (\(a, b) cs -> sliceSeg (toFixedSeg t) a b : cs) [] $ zip isects (tail isects)
      where
        isects         = exactEndpoints . sort . avoidEmptySegs . subSegs $ concat explodedPath
        exactEndpoints = (0:) . (++[1]) . filter (\p -> (p > eps) && (p < 1-eps))
        avoidEmptySegs = nubBy (\a b -> abs (a - b) < eps)
        subSegs        = concatMap $ notOnSelf $ map fst . intersectParamsT' eps t
        notOnSelf f t' = if t' /= t then f t' else [] -- intersecting an FCubic with itself explodes

-----------------------------------------------------------------------------
--  Consuming Exploded Paths ------------------------------------------------
-----------------------------------------------------------------------------
-- | \"Explode\" a path and zip it with a set of transformations before
--    recombining it.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_onExplodedPathEx.svg#diagram=onExplodedPathEx&width=300>>
--
--   > onExplodedPathEx :: _ => Dia b
--   > onExplodedPathEx = onExplodedPath
--   >   (  square 1
--   >   <> square 1 # rotate (1/8 @@ turn) )
--   >   [ [lc red, lc orange, lc yellow, lc green]
--   >   , cycle [lc blue, lc purple] ]
onExplodedPath :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Path V2 n -> [[QDiagram b V2 n Any -> QDiagram b V2 n Any]] -> QDiagram b V2 n Any
onExplodedPath p fs
  = mconcat . mconcat
  . zipWith (zipWith ($)) fs
  . map (map strokeP)
  $ explodePath p

-- | \"Explode\" a path at its intersections and zip it with a set of
--   transformations before recombining it.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_onExplodedIntersectionsEx.svg#diagram=onExplodedIntersectionsEx&width=300>>
--
--   > onExplodedIntersectionsEx :: _ => Dia b
--   > onExplodedIntersectionsEx = onExplodedIntersections
--   >   (  square 1
--   >   <> square 1 # rotate (1/8 @@ turn) )
--   >   [ repeat [lc red, lc orange, lc yellow]
--   >   , let cs = [lc green, lc blue, lc purple]
--   >     in  cycle [cs, reverse cs] ]
onExplodedIntersections :: (TypeableFloat n, Renderable (Path V2 n) b)
  => Path V2 n -> [[[QDiagram b V2 n Any -> QDiagram b V2 n Any]]] -> QDiagram b V2 n Any
onExplodedIntersections = onExplodedIntersections' defEps

-- | `onExplodedIntersections` with intersections calculated within the given
--   tolerance.
onExplodedIntersections' :: (TypeableFloat n, Renderable (Path V2 n) b)
  => n -> Path V2 n -> [[[QDiagram b V2 n Any -> QDiagram b V2 n Any]]] -> QDiagram b V2 n Any
onExplodedIntersections' eps p fs
  = mconcat . mconcat . mconcat
  . zipWith (zipWith (zipWith ($))) fs
  . map (map (map strokeP))
  $ explodeIntersections' eps p
