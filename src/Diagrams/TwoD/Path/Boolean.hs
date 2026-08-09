-- | Set operations on paths.  As a side effect it removes overlapping
-- regions.  Since `Path` is `TrailLike`, you can use these operations
-- directly with any combinator which generates `Loop`s, like `circle`
-- or `fromSegments`.  `Line`s are discarded, only `Loop`s are
-- used. If you have several paths, you can combine them with `<>` first.
-- Use `toPath` if you want to convert a `Trail` or `Located` `Trail`
-- to a `Path`.  The `FillRule` argument determines how /insideness/
-- is calculated for the input.
--
-- This module is deprecated: the implementation now lives in
-- "Geometry.TwoD.Path.Boolean" (polymorphic in the scalar type).  The
-- definitions below are monomorphic (@Double@) specialisations kept
-- for compatibility.

module Diagrams.TwoD.Path.Boolean
       {-# DEPRECATED "Use Geometry.TwoD.Path.Boolean instead" #-}
       (-- * operations on Paths
         union, difference, intersection, exclusion,
         -- * operations on Paths with tolerance
         union', difference', intersection', exclusion',
         -- * operations on Loops
         loopUnion, loopDifference,
         loopIntersection, loopExclusion,)
       where

import           Geometry.Located              (Located)
import           Geometry.Path                 (Path)
import           Geometry.Trail                (Loop)
import qualified Geometry.TwoD.Path.Boolean    as G
import           Linear                        (V2)

import           Diagrams.TwoD.Path            (FillRule (..))

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
union = G.union

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
intersection = G.intersection

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
difference = G.difference

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
exclusion = G.exclusion

-- | Like `union`, but takes a tolerance parameter.
union' :: Double -> FillRule -> Path V2 Double -> Path V2 Double
union' = G.union'

-- | Like `intersection`, but takes a tolerance parameter.
intersection' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
intersection' = G.intersection'

-- | Like `difference`, but takes a tolerance parameter.
difference' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
difference' = G.difference'

-- | Like `exclusion`, but takes a tolerance parameter.
exclusion' :: Double -> FillRule -> Path V2 Double -> Path V2 Double -> Path V2 Double
exclusion' = G.exclusion'

-- | Union of a list of loops.
loopUnion :: Double -> FillRule
          -> [Located (Loop V2 Double)]
          -> [Located (Loop V2 Double)]
loopUnion = G.loopUnion

-- | Difference between loops.  The loops in both lists are first merged using `union`.
loopDifference :: Double -> FillRule
               -> [Located (Loop V2 Double)]
               -> [Located (Loop V2 Double)]
               -> [Located (Loop V2 Double)]
loopDifference = G.loopDifference

-- | Intersection of loops.  The loops in both lists are first merged using `union`.
loopIntersection :: Double -> FillRule
                 -> [Located (Loop V2 Double)]
                 -> [Located (Loop V2 Double)]
                 -> [Located (Loop V2 Double)]
loopIntersection = G.loopIntersection

-- | Exclusion (xor) of loops. The loops in both lists are first merged using `union`.
loopExclusion :: Double -> FillRule
              -> [Located (Loop V2 Double)]
              -> [Located (Loop V2 Double)]
              -> [Located (Loop V2 Double)]
loopExclusion = G.loopExclusion
