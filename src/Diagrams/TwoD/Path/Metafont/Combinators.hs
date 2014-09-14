{-# LANGUAGE TypeFamilies #-}

-- | Combinators to allow writing Metafont-style paths embedded in
-- Haskell, with the usual Diagrams types for points and directions.

module Diagrams.TwoD.Path.Metafont.Combinators
       (
           (.-), (-.), (.--.)
       , endpt, cyclePath
       , simpleJoin -- is this actually needed?
       , tension, tensions, controls
       , leaving, arriving
       ) where

import           Diagrams.Prelude
import           Diagrams.TwoD.Path.Metafont.Types

-- internal alias to keep the signatures readable
type Join n = PathJoin (Maybe (PathDir n)) (Maybe (BasicJoin n))

-- | /point/ @.-@ /join/ @-.@ /path/ adds /point/ to the
-- left end of the metafont /path/, connected by /join/.
(.-) :: P2 n -> MFPathData J n -> MFPathData P n
(.-) = MFPathPt

-- | See @.-@ above.
(-.) :: Join n -> MFPathData P n -> MFPathData J n
(-.) = MFPathJoin

infixr 5 .-
infixr 5 -.

-- | Terminate the right-end of a Metafont path at the given point.
endpt :: P2 n -> MFPathData P n
endpt = MFPathEnd

-- | Wrap the right-end of the Metafont path back to the left-end.
-- When converted to a Diagrams 'Trail'', this will be a Loop.
cyclePath :: MFPathData P n
cyclePath = MFPathCycle

-- | Add a point to the left of a Metafont path using a simple join.
-- That is, neither direction is specified, and both tensions are 1.
(.--.) :: P2 n -> MFPathData P n -> MFPathData P n
p .--. q = p .- mempty -. q

infixr 5 .--.

-- | simpleJoin is the same as mempty, with a more specific type.  It
-- is provided for convenience in situations where explicit type
-- signatures would otherwise be needed, such as when building up a
-- join using lenses.
simpleJoin :: Join n
simpleJoin = mempty

-- | A join with both tensions the same.
tension :: n -> Join n
tension t = PJ Nothing (Just . Left $ TJ (TensionAmt t) (TensionAmt t)) Nothing

-- | A join with two tension values.
tensions :: n -> n -> Join n
tensions tl tr = PJ Nothing (Just . Left $ TJ (TensionAmt tl) (TensionAmt tr)) Nothing

-- | A join with explicit control points.  Note that these are in the
-- same coordinate system as the endpoints, not relative to the latter.
controls :: P2 n -> P2 n -> Join n
controls u v = simpleJoin & j.~ (Just . Right $ CJ u v)

-- | A join with the left-end direction specified.
leaving :: V2 n -> Join n
leaving d = mempty & d1.~ (Just . PathDirDir $ d)

-- | A join with the right-end direction specified.
arriving :: V2 n -> Join n
arriving d = mempty & d2.~ (Just . PathDirDir $ d)
