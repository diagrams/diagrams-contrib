{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , intersectParamsTS, intersectParamsTS'
    -- * Cutting Paths and Trails
  , cutBy, cutBy'
  , cutPBy, cutPBy'
  , cutTBy, cutTBy'
    -- * Rad Explosions
  , explodeSegments
  , explodeIntersections, explodeIntersections'
  , explodeBoth, explodeBoth'
    -- * Consuming Cut Paths
  , OnSections(..)
  ) where
import Data.List

import Diagrams.Prelude
import Diagrams.TwoD.Segment

-- defEps uses the value from Diagrams.TwoD.Path
defEps :: Fractional n => n
defEps = 1e-8

-----------------------------------------------------------------------------
-- Intersection Parameters --------------------------------------------------
-----------------------------------------------------------------------------

-- | Find the intersect parameters for each component trail of two pathlike
--   objects when the objects are intersected, returning a seperate list for
--   each trail.
intersectParams :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) =>
  t -> s -> ([[n]], [[n]])
intersectParams = intersectParams' defEps

-- | `intersectParams` using the given tolerance.
intersectParams' :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n) =>
  n -> t -> s -> ([[n]], [[n]])
intersectParams' eps as bs = intersectParamsP' eps (toPath as) (toPath bs)

-- | Find the intersect parameters for each component trail of two
--   paths when the paths are intersected, returning a seperate list for
--   each trail.
intersectParamsP :: OrderedField n => Path V2 n -> Path V2 n -> ([[n]], [[n]])
intersectParamsP = intersectParamsP' defEps

-- | `intersectParamsP` using the given tolerance.
intersectParamsP' :: OrderedField n => n -> Path V2 n -> Path V2 n -> ([[n]], [[n]])
intersectParamsP' eps as bs = (ps, qs)
  where
    is = map (flip map (pathTrails bs) . intersectParamsT' eps) (pathTrails as)
    ps = map (concat . map fst) is
    qs = map (concat . map snd) (transpose is)

-- | Find the intersect parameters between two located trails.
intersectParamsT :: OrderedField n =>
  Located (Trail V2 n) -> Located (Trail V2 n) -> ([n], [n])
intersectParamsT = intersectParamsT' defEps

-- | `intersectParamsT` using the given tolerance.
intersectParamsT' :: OrderedField n =>
  n -> Located (Trail V2 n) -> Located (Trail V2 n) -> ([n], [n])
intersectParamsT' eps as bs = (reparam ps, reparam qs)
  where
    (ps, qs) = intersectParamsTS' eps as bs
    reparam segs = concat $ zipWith f [(0::Int)..] segs
      where f segNo = map $ \p -> (fromIntegral segNo + p) / genericLength segs

-- | Find the intersect parameters for each component segment of two
--   located trails when the trails are intersected, returning a
--   list for each trail containing a list of intersections for
--   each segemnt of that trail.
intersectParamsTS :: OrderedField n =>
  Located (Trail V2 n) -> Located (Trail V2 n) -> ([[n]], [[n]])
intersectParamsTS = intersectParamsTS' defEps

-- | `intersectParamsTS` using the given tolerance.
intersectParamsTS' :: OrderedField n =>
  n -> Located (Trail V2 n) -> Located (Trail V2 n) -> ([[n]], [[n]])
intersectParamsTS' eps as bs = (ps, qs)
  where
    (as', bs') = (as, bs) & both %~ (zip [0..] . fixTrail)
    is = map (flip map bs' . isect) as'
    isect (i, a) (j, b)
      | a == b    = []
      | otherwise = filter (not . ends)
                  . map (\(p, q, _) -> (p, q))
                  $ segmentSegment eps a b
      where
        ends (p, q) = adjacent && min p q `near` 0 && max p q `near` 1
        adjacent = as == bs && (abs (i - j) == 1 || min i j == 0 && max i j == length as' - 1)
        near x n = abs (x - n) < eps
    ps = map (map fst . concat) is
    qs = map (map snd . concat) (transpose is)

-----------------------------------------------------------------------------
-- Cutting Paths and Trails -------------------------------------------------
-----------------------------------------------------------------------------

-- | Seperate a pathlike object into sections at every point it intersects
--   a second pathlike object, returning a list of sections for each component
--   trail.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_cutByEx.svg#diagram=cutByEx&width=300>>
--
--   > cutByEx = onSections (squares `cutBy` line) colorLines
--   >        <> stroke line
--   >   where
--   >     squares, line :: Path V2 Double
--   >     squares = square 1
--   >            <> square 1 # rotate (1/8 @@ turn)
--   >     line  = hrule 2
--   >     colorLines = map (map lc)
--   >       [ [ red, orange]
--   >       , [blue, purple] ]
cutBy :: (OrderedField n, Real n, InSpace V2 n t, SameSpace t s, ToPath t, ToPath s) =>
  t -> s -> [[Located (Trail V2 n)]]
cutBy = cutBy' defEps

-- | `cutBy` using the given tolerance for calculating intersections.
cutBy' :: (OrderedField n, Real n, InSpace V2 n t, SameSpace t s, ToPath t, ToPath s) =>
  n -> t -> s -> [[Located (Trail V2 n)]]
cutBy' eps a b = cutPBy' eps (toPath a) (toPath b)

-- | Seperate a path into sections at every point it intersects a second path,
--   returning a list of sections for each component trail.
cutPBy :: (OrderedField n, Real n) => Path V2 n -> Path V2 n -> [[Located (Trail V2 n)]]
cutPBy = cutPBy' defEps

-- | `cutPBy` using the given tolerance for calculating intersections.
cutPBy' :: (OrderedField n, Real n) => n -> Path V2 n -> Path V2 n -> [[Located (Trail V2 n)]]
cutPBy' eps p1 p2 = map (flip (cutTBy' eps) p2) (pathTrails p1)

-- | Seperate a located trail into sections at every point it intersects a path.
cutTBy :: (OrderedField n, Real n) => Located (Trail V2 n) -> Path V2 n -> [Located (Trail V2 n)]
cutTBy = cutTBy' defEps

-- | `cutTBy` using the given tolerance for calculating intersections.
cutTBy' :: (OrderedField n, Real n) => n -> Located (Trail V2 n) -> Path V2 n -> [Located (Trail V2 n)]
cutTBy' eps t p
  | null isects                                 = [t]
  | null nearEnds && norm (start .-. end) < eps = gluedEnds
  | otherwise                                   = subsections
  where
    subsections = zipWith (section t) (0:isects) (isects++[1])
    isects = sortAndAvoidEmpty notNearEnds
    sortAndAvoidEmpty = map head . groupBy (\a b -> abs (a - b) < eps) . sort
    (notNearEnds, nearEnds) = partition (\p -> (eps < p) && (p < 1-eps)) rawIsects
    rawIsects = concatMap (fst . intersectParamsT' eps t) (pathTrails p)

    start = head subsections `atParam` 0
    end   = last subsections `atParam` 1
    gluedEnds = unfixTrail (fixTrail (last subsections) ++ fixTrail (head subsections))
              : init (tail subsections)

-----------------------------------------------------------------------------
-- Rad Explosions -----------------------------------------------------------
-----------------------------------------------------------------------------

-- | explodePath specialized to return located trails. This provides the compiler
--   the necessary type information to use it with `onSections` without providing
--   a type annotation.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_explodeSegmentsEx.svg#diagram=explodeSegmentsEx&width=300>>
--
--   > explodeSegmentsEx = onSections (explodeSegments squares) colorLines
--   >   where
--   >     squares = square 1
--   >            <> square 1 # rotate (1/8 @@ turn)
--   >     colorLines = map (map lc)
--   >       [ [ red, yellow,   gold, orange]
--   >       , [blue, violet, purple, indigo] ]
explodeSegments :: (Metric v, OrderedField n) => Path v n -> [[Located (Trail v n)]]
explodeSegments = explodePath

-- | Turn a path a list of component trails, then cut those segments at all
--   their intersections.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_explodeIntersectionsEx.svg#diagram=explodeIntersectionsEx&width=300>>
--
--   > explodeIntersectionsEx = onSections (explodeIntersections squares) colorLines
--   >   where
--   >     squares = square 1
--   >            <> square 1 # rotate (1/8 @@ turn)
--   >     colorLines = map (map lc)
--   >       [ [ gray,     red,     orange, yellow,     green,     blue,       indigo,     violet]
--   >       , [black, crimson, darkorange,   gold, darkgreen, darkblue, midnightblue, darkviolet] ]
explodeIntersections :: (OrderedField n, Real n)  => Path V2 n -> [[Located (Trail V2 n)]]
explodeIntersections = explodeIntersections' defEps

-- | `explodeIntersections` using the given tolerance for calculating intersections.
explodeIntersections' :: (OrderedField n, Real n) => n -> Path V2 n -> [[Located (Trail V2 n)]]
explodeIntersections' eps path = cutBy' eps path path

-- | Turn a path into a list of component segments for each component trail,
--   then cut those segments at all their intersections.
--
--   <<diagrams/src_Diagrams_TwoD_Path_IntersectionExtras_explodeBothEx.svg#diagram=explodeBothEx&width=300>>
--
--   > explodeBothEx = onSections (explodeBoth squares) colorLines
--   >   where
--   >     squares = square 1
--   >            <> square 1 # rotate (1/8 @@ turn)
--   >     colorLines = map (map (map lc))
--   >       [ cycle [ [ gray,     red,     orange], [yellow,     green,     blue] ]
--   >       , cycle [ [black, crimson, darkorange], [  gold, darkgreen, darkblue] ] ]
explodeBoth :: (OrderedField n, Real n) => Path V2 n -> [[[Located (Trail V2 n)]]]
explodeBoth = explodeBoth' defEps

-- | `explodeBoth` using the given tolerance for calculating intersections.
explodeBoth' :: (OrderedField n, Real n) => n -> Path V2 n -> [[[Located (Trail V2 n)]]]
explodeBoth' eps path = map (map (flip (cutTBy' eps) path)) $ explodePath path

-----------------------------------------------------------------------------
--  Consuming Cut Paths -----------------------------------------------------
-----------------------------------------------------------------------------
class OnSections ps fs b n | ps b -> fs n, fs -> b n where
  -- | Zipply apply an arbitrarily nested list of attributes to the same shape
  --   of lists of pathlike objects, monoidally combining the results.
  --
  --   See examples for `cutBy`, `explodeSegments`, `explodeIntersections`, and `explodeBoth`.
  onSections :: ps -> fs -> QDiagram b V2 n Any

-- Need to list out the instances rather than using overlaping instances
-- with ToPath in order to use the fundep (ps b -> fs).

instance (TypeableFloat n, OnSections ps fs b n) =>
  OnSections [ps] [fs] b n where
  onSections ps fs = mconcat $ zipWith onSections ps fs

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Path V2 n) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Located (Trail V2 n)) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Located (Trail' l V2 n)) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Located [Segment Closed V2 n]) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Located (Segment Closed V2 n)) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Trail V2 n) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (Trail' l V2 n) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (FixedSegment V2 n) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs $ stroke ps

instance (TypeableFloat n, Renderable (Path V2 n) b) =>
  OnSections (QDiagram b V2 n Any) (QDiagram b V2 n Any -> QDiagram b V2 n Any) b n where
  onSections ps fs = fs ps
