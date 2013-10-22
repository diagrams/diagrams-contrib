{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- |
-- Module      :  Diagrams.Lens
-- Copyright   :  (c) 2013 Michael Sloan
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
--
-- This module provides utilities for using "Control.Lens" with diagrams,
-- including orphan instances for the 'Wrapped' class.
module Diagrams.Lens
  ( Wrapped'
  , _P
  -- * Diagrams.Align
  , _envelopeVMove
  , _alignedVMove
  -- * Diagrams.BoundingBox
  , _corners
  , _boxExtents
  -- * Diagrams.Core.Style
  , _attr
  , _mkAttr
  , _mkTAttr
  -- * Diagrams.Core.Types
  , _location
  , _mkSubdiagram
  -- * Diagrams.Located
  , _Loc
  -- * Diagrams.Parametric
  -- , _arcLength
  -- * Diagrams.Segment
  , _mkFixedSeg
  , _straight
  , _bezier3
  -- * Diagrams.Trail
  , _lineVertices
  , _lineOffsets
  , _lineSegments
  -- * Diagrams.TwoD.Types
  , _toTurn
  ) where

import Control.Applicative
import Control.Lens
import Data.AffineSpace.Point (Point(P))
import Data.Basis
import Diagrams.Prelude
import Diagrams.Core.Style
import Diagrams.ThreeD.Types

type Wrapped' s a = Wrapped s s a a

$(concat <$> mapM makeWrapped
  [ ''Deg
  , ''R3
  , ''Rad
  , ''Turn
--TODO: re-introduce - this is probably a bug in 'makeWrapped'
--  , ''SubMap
--  , ''Path
--  , ''Point
--  , ''QDiagram
  ])

instance Wrapped [Located (Trail v)]  [Located (Trail v)] (Path v) (Path v) where
  wrapped = from pathTrails
  {-# INLINE wrapped #-}

instance Wrapped v v (Point v) (Point v) where
  wrapped = _P
  {-# INLINE wrapped #-}

instance Wrapped (Double, Double) (Double, Double) R2 R2 where
  wrapped = iso r2 unr2
  {-# INLINE wrapped #-}

_P :: Iso s t (Point s) (Point t)
_P = iso P $ \(P x) -> x

-- * Diagrams.Align

-- | A singleton 'Traversal' for an envelope vector, where modification of the
--   vector moves the origin appropriately.  No vector is traversed when the
--   envelope is empty.
--
--   This is the same as '_alignedVMove' with @1@ as the interpolation
--   parameter.
_envelopeVMove
  :: (Monoid a, HasOrigin a, Enveloped a, Num (Scalar (V a)))
  => V a -> Traversal' a (V a)
_envelopeVMove v f x = case envelopeVMay v x of
  (Just p) -> (\p' -> moveOriginBy (p ^-^ p') x) <$> f p
  Nothing -> pure x

-- | A singleton 'Traversal' for an alignment vector, where modification of the
--   vector moves the origin appropriately.  No vector is traversed when the
--   envelope is empty.
--
--   The interface mimics "Diagrams.Align.alignBy" in that the @d@ parameter
--   specifies an interpolation between two extremes of the envelope. @d = 1@
--   is on the envelope along the vector, whereas @d = -1@ is on the envelope,
--   away from the vector.
--
--   If you need a 'Point' instead of a vector, then compose with '_P'.
_alignedVMove
  :: (Monoid a, HasOrigin a, Enveloped a, Num (Scalar (V a)))
  => V a -> Scalar (V a) -> Traversal' a (V a)
_alignedVMove v d f x = case appEnvelope $ getEnvelope x of
  (Just env) -> (\p' -> moveOriginBy (p ^-^ p') x) <$> f p
    where
      p = v ^* lerp' (env (negateV v)) (env v) ((d + 1) / 2)
      -- Constraints were pretty wacky otherwise.
      lerp' l u t = (1 - t) * u + t * l
  Nothing -> pure x

{- TODO
_traceMove :: Point v -> v -> Traversal (QDiagram b v m) (Scalar v)
_traceMove p v f x = case appTrace (trace x) p v of
  Finite t ->
  PosInfty -> pure x
-}


-- * Diagrams.BoundingBox

-- | A traversal that either has 0 (empty box) or 2 points.  These points are
--   the lower and upper corners, respectively.
_corners
  :: ( HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v)
     , HasBasis v', Ord (Basis v'), AdditiveGroup (Scalar v'), Ord (Scalar v'))
  => Traversal (BoundingBox v) (BoundingBox v') (Point v) (Point v')
_corners f (getCorners -> Just (l, t)) = fromCorners <$> f l <*> f t
_corners _ _ = pure emptyBox

-- | A lens that gets the extents of the box.  In order to change the extents,
--   this modifies the upper corner.
_boxExtents
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Lens' (BoundingBox v) v
_boxExtents = lens boxExtents setExtent
  where
    setExtent (getCorners -> Just (l, _)) x = fromCorners l (l .+^ x)
    setExtent _ _ = emptyBox


-- * Diagrams.Core.Style

_attr :: AttributeClass a => Lens' (Style v) (Maybe a)
_attr = lens getAttr setAttr'
  where
    setAttr' style (Just x) = setAttr x style
    setAttr' style Nothing = style

_mkAttr
  :: AttributeClass a => Prism' (Attribute v) a
_mkAttr = prism' mkAttr unwrapAttr

_mkTAttr
  :: (AttributeClass a, Transformable a, V a ~ v)
  => Prism' (Attribute v) a
_mkTAttr = prism' mkTAttr unwrapAttr


-- * Diagrams.Core.Types

-- | Gets or set the 'location' of a 'Subdiagram'.
_location
  :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
  => Lens' (Subdiagram b v m) (Point v)
--TODO: Is this correct??
_location = lens location (flip Diagrams.Prelude.moveTo)

_mkSubdiagram
  :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Semigroup m)
  => Iso' (QDiagram b v m) (Subdiagram b v m)
_mkSubdiagram = iso mkSubdiagram getSub


-- * Diagrams.Located

_Loc :: Iso (Located a) (Located a') (Point (V a), a) (Point (V a'), a')
_Loc = iso viewLoc (uncurry $ flip Diagrams.Prelude.at)


-- * Diagrams.Parametric

{- TODO: requires 'arcLengthFromParam'

_arcLength
  :: HasArcLength p => Scalar (V p) -> p -> Iso' (Scalar (V p)) (Scalar (V p))
_arcLength eps curve
  = iso' (arcLengthFromParam eps curve) (arcLengthToParam eps curve)

-}

{-}
_arcLength
  :: (s ~ Scalar (V p), HasArcLength p, Sectionable p, Fractional s)
  => AdjustSide -> s -> Lens' p s
_arcLength side eps = lens (arcLength eps) adjustArcLength
  where
    adjustArcLength s x = adjust s AO
      { _adjMethod = ToAbsolute x
      , _adjSide = side
      , _adjEps = eps
      }
-}
-- * Diagrams.Segment

_mkFixedSeg
  :: (AdditiveGroup v, AdditiveGroup v')
  => Iso
    (Located (Segment Closed v))
    (Located (Segment Closed v'))
    (FixedSegment v)
    (FixedSegment v')
_mkFixedSeg = iso mkFixedSeg fromFixedSeg

-- | Prism that constructs linear segments.  Can also destruct them, if the
--   segment is Linear.
_straight :: Prism' (Segment Closed v) v
_straight = prism' straight fromStraight
  where
    fromStraight :: Segment c a -> Maybe a
    fromStraight (Linear (OffsetClosed x)) = Just x
    fromStraight _ = Nothing

-- | Prism that constructs cubic bezier segments.  Can also destruct them, if
--   segment is a 'Cubic'.
_bezier3 :: Prism' (Segment Closed v) (v, v, v)
_bezier3 = prism' (\(c1, c2, c3) -> bezier3 c1 c2 c3) fromBezier3
  where
    fromBezier3 :: Segment c a -> Maybe (a, a, a)
    fromBezier3 (Cubic c1 c2 (OffsetClosed c3)) = Just (c1, c2, c3)
    fromBezier3 _ = Nothing


-- * Diagrams.Trail

_lineVertices
  :: ( InnerSpace v, OrderedField (Scalar v)
     , InnerSpace v', OrderedField (Scalar v'))
  => Iso
    (Located (Trail' Line v)) (Located (Trail' Line v'))
    [Point v] [Point v']
_lineVertices = iso lineVertices fromVertices

_lineOffsets
  :: ( InnerSpace v, OrderedField (Scalar v)
     , InnerSpace v', OrderedField (Scalar v'))
  => Iso
    (Trail' Line v) (Trail' Line v')
    [v] [v']
_lineOffsets = iso lineOffsets lineFromOffsets

_lineSegments
  :: ( InnerSpace v, OrderedField (Scalar v)
     , InnerSpace v', OrderedField (Scalar v'))
  => Iso
    (Trail' Line v) (Trail' Line v')
    [Segment Closed v] [Segment Closed v']
_lineSegments = iso lineSegments lineFromSegments


-- * Diagrams.TwoD.Types

-- | 'toTurn' is an isomorphism from angles to 'Turn's.
_toTurn :: (Angle a, Angle a') => Iso a a' Turn Turn
_toTurn = iso toTurn fromTurn
