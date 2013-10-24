{-# LANGUAGE TypeFamilies #-}

module Diagrams.TwoD.Path.Metafont.Combinators where

import Diagrams.Prelude
import Diagrams.TwoD.Path.Metafont.Types

-- internal alias to keep the signatures readable
type Join = PathJoin (Maybe PathDir) (Maybe BasicJoin)


-- | /point/ @.-@ /join/ @-.@ /path/ adds /point/ to the
-- left end of the metafont /path/, connected by /join/.

(.-) :: P2 -> MFPathData J -> MFPathData P
(.-) = MFPathPt

-- | See @.-@ above.
(-.) :: Join -> MFPathData P -> MFPathData J
(-.) = MFPathJoin

infixr 5 .-
infixr 5 -.

-- | Terminate the right-end of a Metafont path at the given point.
endpt :: P2 -> MFPathData P
endpt = MFPathEnd

-- | Wrap the right-end of the Metafont path back to the left-end.
-- When converted to a Diagrams 'Trail'', this will be a Loop.
cyclePath :: MFPathData P
cyclePath = MFPathCycle

-- | Add a point to the left of a Metafont path using a simple join.
-- That is, neither direction is specified, and both tensions are 1.
(.--.) :: P2 -> MFPathData P -> MFPathData P
p .--. q = p .- mempty -. q

infixr 5 .--.

-- | simpleJoin is the same as mempty, with a more specific type.  It
-- is provided for convenience in situations where explicit type
-- signatures would otherwise be needed, such as when building up a
-- join using lenses.
simpleJoin :: Join
simpleJoin = mempty

-- | A join with both tensions the same.
tension :: Double -> Join
tension t = PJ Nothing (Just . Left $ TJ (TensionAmt t) (TensionAmt t)) Nothing

-- | A join with two tension values.
tensions :: Double -> Double -> Join
tensions tl tr = PJ Nothing (Just . Left $ TJ (TensionAmt tl) (TensionAmt tr)) Nothing

-- | A join with explicit control points.  Note that these are in the
-- same coordinate system as the endpoints, not relative to the latter.
controls :: P2 -> P2 -> Join
controls u v = (mempty :: Join) & j.~ (Just . Right $ CJ u v)

-- | A join with the left-end direction specified.
leaving :: R2 -> Join
leaving d = mempty & d1.~ (Just . PathDirDir $ d)

-- | A join with the right-end direction specified.
arriving :: R2 -> Join
arriving d = mempty & d2.~ (Just . PathDirDir $ d)
