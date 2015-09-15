{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Set operations on paths.  As a side effect it removes overlapping
-- regions.  Since `Path` is `TrailLike`, you can use these operations
-- directly with any combinator which generates `Loop`s, like `circle`
-- or `fromSegments`.  `Line`s are discarded, only `Loop`s are
-- used. If you have several paths, you can combine them with `<>` first.
-- Use `toPath` if you want to convert a `Trail` or `Located` `Trail`
-- to a `Path`.  The `FillRule` argument determines how /insideness/
-- is calculated for the input.

module Diagrams.TwoD.Path.Boolean
       (-- * operations on Paths
         union, difference, intersection, exclusion,
         -- * operations on Paths with tolerance
         union', difference', intersection', exclusion',
         -- * operations on Loops
         loopUnion, loopDifference,
         loopIntersection, loopExclusion,)
       where
import Diagrams.Located
import Diagrams.Trail
import Diagrams.TrailLike
import Control.Lens hiding (at)
import Linear
import Diagrams.Points
import Diagrams.Segment
import Diagrams.TwoD.Path
import Diagrams.Path
import Data.Maybe
import qualified Geom2D.CubicBezier as C

fillrule :: FillRule -> C.FillRule
fillrule Winding = C.NonZero
fillrule EvenOdd = C.EvenOdd

loop2path :: Located (Trail' Loop V2 Double) -> C.ClosedPath Double
loop2path t =
  C.ClosedPath $ go x0 y0 (lineSegments $ cutLoop $ unLoc t)
  where
    (P (V2 x0 y0)) = loc t
    go :: Double -> Double -> [Segment Closed V2 Double] -> [(C.DPoint, C.PathJoin Double)]
    go _ _ [] = []
    go x y (Linear (OffsetClosed (V2 x3 y3)):r) =
      (C.Point x y, C.JoinLine) :
      go (x+x3) (y+y3) r
    go x y (Cubic (V2 x1 y1) (V2 x2 y2) (OffsetClosed (V2 x3 y3)):r) =
      (C.Point x y, C.JoinCurve (C.Point (x+x1) (y+y1)) (C.Point (x+x2) (y+y2))) :
      go (x+x3) (y+y3) r

path2loop :: C.ClosedPath Double -> Located (Trail' Loop V2 Double)
path2loop (C.ClosedPath []) = fromSegments [] `at` origin
path2loop (C.ClosedPath ((C.Point x0 y0, join):r)) =
  fromSegments (go x0 y0 join r) `at` P (V2 x0 y0)
  where go x y C.JoinLine [] =
          [straight (V2 (x0-x) (y0-y))]
        go x y C.JoinLine ((C.Point x2 y2, join'):r') =
          straight (V2 (x2-x) (y2-y)):
          go x2 y2 join' r'
        go x y (C.JoinCurve (C.Point x1 y1) (C.Point x2 y2)) r' =
          case r' of
           [] -> [bezier3 (V2 (x1-x) (y1-y))
                  (V2 (x2-x) (y2-y)) (V2 (x0-x) (y0-y))]
           ((C.Point x3 y3, join'):r'') ->
             bezier3 (V2 (x1-x) (y1-y)) (V2 (x2-x) (y2-y))
             (V2 (x3-x) (y3-y)) :
             go x3 y3 join' r''

trail2loop :: Located (Trail V2 Double) -> Maybe (Located (Trail' Loop V2 Double))
trail2loop = located (withTrail (const Nothing) Just)

offsetMax :: Offset c V2 Double -> Double
offsetMax (OffsetClosed (V2 m n)) = max (abs m) (abs n)
offsetMax OffsetOpen = 0

segmentMax :: Segment c V2 Double -> Double
segmentMax (Linear o) =
  offsetMax o
segmentMax (Cubic (V2 a b) (V2 c d) o) =
  maximum [offsetMax o, abs a, abs b,
           abs c, abs d]

loopMax :: Trail' Loop V2 Double -> Double
loopMax l = maximum (segmentMax lastSeg: map segmentMax segs)
  where (segs, lastSeg) = loopSegments l
  
defaultTol :: Double
defaultTol = 1e-7

loop2trail :: Located (Trail' Loop V2 Double) -> Located (Trail V2 Double)
loop2trail = over located wrapLoop

-- | Remove overlapping regions in the path.  If you have several
-- paths, combine them using `<>` first.
--
-- <<diagrams/src_Diagrams_TwoD_Path_Boolean_unionEx.svg#diagram=unionEx&width=300>>
--
-- > import Diagrams.TwoD.Path.Boolean
-- > import Diagrams.Prelude hiding (union)
-- > 
-- > unionEx = strokePath $ union Winding $
-- >           (square 1) <> circle 0.5 # translate (V2 0.5 (-0.5))

union :: FillRule -> Path V2 Double -> Path V2 Double
union fill p =
  Path $ map loop2trail $ 
  loopUnion tol fill loops
  where loops = mapMaybe trail2loop $
                pathTrails p
        tol = maximum (map (loopMax.unLoc) loops) *
              defaultTol

-- | Intersection of two paths.  First overlap is removed in the two
-- input arguments, then the intersection is calculated.
-- 
-- <<diagrams/src_Diagrams_TwoD_Path_Boolean_isectEx.svg#diagram=isectEx&width=200>>
--
-- > import Diagrams.TwoD.Path.Boolean
-- > import Diagrams.Prelude hiding (intersection)
-- >
-- > isectEx = strokePath $
-- >           intersection Winding (square 1) $
-- >           circle 0.5 # translate (V2 0.5 (-0.5))
intersection :: FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
intersection fill path1 path2 =
  Path $ map loop2trail $
  loopIntersection tol fill loops1 loops2
  where loops1 = mapMaybe trail2loop $
                pathTrails path1
        loops2 = mapMaybe trail2loop $
                pathTrails path2
        tol = max (maximum (map (loopMax.unLoc) loops1))
              (maximum (map (loopMax.unLoc) loops2))
              * defaultTol
-- | Difference of two paths.  First overlap is removed in the two
-- input arguments, then the difference is calculated.
--
-- <<diagrams/src_Diagrams_TwoD_Path_Boolean_diffEx.svg#diagram=diffEx&width=200>>
-- 
-- > import Diagrams.TwoD.Path.Boolean
-- > import Diagrams.Prelude hiding (difference)
-- >
-- > diffEx = strokePath $
-- >          difference Winding (square 1) $
-- >          circle 0.5 # translate (V2 0.5 (-0.5))
difference :: FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
difference fill path1 path2 =
  Path $ map loop2trail $
  loopDifference tol fill loops1 loops2
  where loops1 = mapMaybe trail2loop $
                pathTrails path1
        loops2 = mapMaybe trail2loop $
                pathTrails path2
        tol = max (maximum (map (loopMax.unLoc) loops1))
              (maximum (map (loopMax.unLoc) loops2))
              * defaultTol

-- | Exclusion (exclusive or) of two paths.  First overlap is removed in the two
-- input arguments, then the exclusion is calculated.
--
-- <<diagrams/src_Diagrams_TwoD_Path_Boolean_exclEx.svg#diagram=exclEx&width=200>>
--
-- > import Diagrams.TwoD.Path.Boolean
-- >
-- > exclEx = fc grey $ strokePath $
-- >          exclusion Winding (square 1) $
-- >          circle 0.5 # translate (V2 0.5 (-0.5))
exclusion :: FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
exclusion fill path1 path2 =
  Path $ map loop2trail $
  loopExclusion tol fill loops1 loops2
  where loops1 = mapMaybe trail2loop $
                pathTrails path1
        loops2 = mapMaybe trail2loop $
                pathTrails path2
        tol = max (maximum (map (loopMax.unLoc) loops1))
              (maximum (map (loopMax.unLoc) loops2))
              * defaultTol

-- | Like `union`, but takes a tolerance parameter.
union' :: Double -> FillRule -> Path V2 Double -> Path V2 Double
union' tol fill p =
  Path $ map loop2trail $ 
  loopUnion tol fill $
  mapMaybe trail2loop $
  pathTrails p

-- | Like `intersection`, but takes a tolerance parameter.
intersection' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
intersection' tol fill path1 path2 =
  Path $ map loop2trail $
  loopIntersection tol fill
  (mapMaybe trail2loop $ pathTrails path1)
  (mapMaybe trail2loop $ pathTrails path2)


-- | Like `difference`, but takes a tolerance parameter.
difference' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
difference' tol fill path1 path2 =
  Path $ map loop2trail $
  loopDifference tol fill
  (mapMaybe trail2loop $ pathTrails path1)
  (mapMaybe trail2loop $ pathTrails path2)

-- | Like `exclusion`, but takes a tolerance parameter.
exclusion' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
exclusion' tol fill path1 path2 =
  Path $ map loop2trail $
  loopExclusion tol fill
  (mapMaybe trail2loop $ pathTrails path1)
  (mapMaybe trail2loop $ pathTrails path2)

-- | Union of a list of loops.
loopUnion :: Double -> FillRule
          -> [Located (Trail' Loop V2 Double)]
          -> [Located (Trail' Loop V2 Double)]
loopUnion tol fill p  =
  map path2loop $ C.union (map loop2path p) (fillrule fill) tol

-- | Difference between loops.  The loops in both lists are first merged using `union`.
loopDifference :: Double -> FillRule
               -> [Located (Trail' Loop V2 Double)]
               -> [Located (Trail' Loop V2 Double)]
               -> [Located (Trail' Loop V2 Double)]
loopDifference tol fill path1 path2  =
  map path2loop $ C.difference (map loop2path path1)
  (map loop2path path2) (fillrule fill) tol

-- | Intersection of loops.  The loops in both lists are first merged using `union`.
loopIntersection :: Double -> FillRule
                 -> [Located (Trail' Loop V2 Double)]
                 -> [Located (Trail' Loop V2 Double)]
                 -> [Located (Trail' Loop V2 Double)]
loopIntersection tol fill path1 path2 =
  map path2loop $ C.intersection (map loop2path path1)
  (map loop2path path2) (fillrule fill) tol

-- | Exclusion (xor) of loops. The loops in both lists are first merged using `union`.
loopExclusion :: Double -> FillRule
              -> [Located (Trail' Loop V2 Double)]
              -> [Located (Trail' Loop V2 Double)]
              -> [Located (Trail' Loop V2 Double)]
loopExclusion tol fill path1 path2 =
  map path2loop $ C.exclusion (map loop2path path1)
  (map loop2path path2) (fillrule fill) tol

