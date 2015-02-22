{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
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
  (
  -- * Diagrams.Align
  _envelopeVMove
  , _alignedVMove
  -- * Diagrams.BoundingBox
  , _corners
  , _boxExtents
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
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Diagrams.BoundingBox
import           Diagrams.Core.Style
import           Diagrams.Prelude

-- * Diagrams.Align

-- | A singleton 'Traversal' for an envelope vector, where modification of the
--   vector moves the origin appropriately.  No vector is traversed when the
--   envelope is empty.
--
--   This is the same as '_alignedVMove' with @1@ as the interpolation
--   parameter.
_envelopeVMove
  :: (Monoid a, HasOrigin a, Enveloped a, Num (N a))
  => Vn a -> Traversal' a (Vn a)
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
    :: (Monoid a, HasOrigin a, Enveloped a, Num (N a))
  => Vn a -> N a -> Traversal' a (Vn a)
_alignedVMove v d f x = case appEnvelope $ getEnvelope x of
  (Just env) -> (\p' -> moveOriginBy (p ^-^ p') x) <$> f p
    where
      p = v ^* lerp' (env (negated v)) (env v) ((d + 1) / 2)
      -- Constraints were pretty wacky otherwise.
      lerp' l u t = (1 - t) * u + t * l
  Nothing -> pure x

{- TODO
_traceMove :: Point v -> v -> Traversal (QDiagram b v n m) n
_traceMove p v f x = case appTrace (trace x) p v of
  Finite t ->
  PosInfty -> pure x
-}


-- * Diagrams.BoundingBox

-- | A traversal that either has 0 (empty box) or 2 points.  These points are
--   the lower and upper corners, respectively.
_corners
    :: (Additive v', Foldable v', Ord n')
       => Traversal (BoundingBox v n) (BoundingBox v' n') (Point v n) (Point v' n')
_corners f (getCorners -> Just (l, t)) = fromCorners <$> f l <*> f t
_corners _ _ = pure emptyBox

-- | A lens that gets the extents of the box.  In order to change the extents,
--   this modifies the upper corner.
_boxExtents
  :: (Additive v, Foldable v, Num n, Ord n)
  => Lens' (BoundingBox v n) (v n)
_boxExtents = lens boxExtents setExtent
  where
    setExtent (getCorners -> Just (l, _)) x = fromCorners l (l .+^ x)
    setExtent _ _ = emptyBox


-- * Diagrams.Core.Types

-- | Gets or set the 'location' of a 'Subdiagram'.
_location
  :: (HasLinearMap v, Metric v, OrderedField n)
  => Lens' (Subdiagram b v n m) (Point v n)
--TODO: Is this correct??
_location = lens location (flip Diagrams.Prelude.moveTo)

_mkSubdiagram
  :: (HasLinearMap v, Metric v, OrderedField n, Semigroup m)
  => Iso' (QDiagram b v n m) (Subdiagram b v n m)
_mkSubdiagram = iso mkSubdiagram getSub


-- * Diagrams.Located

_Loc :: Iso (Located a) (Located a') (Point (V a) (N a), a) (Point (V a') (N a'), a')
_Loc = iso viewLoc (uncurry $ flip Diagrams.Prelude.at)


-- * Diagrams.Parametric

{- TODO: requires 'arcLengthFromParam'

_arcLength
  :: HasArcLength p => N p -> p -> Iso' (N p)  (N p)
_arcLength eps curve
  = iso' (arcLengthFromParam eps curve) (arcLengthToParam eps curve)

-}

-- * Diagrams.Segment

_mkFixedSeg
  :: (Additive v, Additive v', Num n, Num n')
  => Iso
    (Located (Segment Closed v n))
    (Located (Segment Closed v' n'))
    (FixedSegment v n)
    (FixedSegment v' n')
_mkFixedSeg = iso mkFixedSeg fromFixedSeg

-- | Prism that constructs linear segments.  Can also destruct them, if the
--   segment is Linear.
_straight :: Prism' (Segment Closed v n) (v n)
_straight = prism' straight fromStraight
  where
    fromStraight :: Segment c v n -> Maybe (v n)
    fromStraight (Linear (OffsetClosed x)) = Just x
    fromStraight _ = Nothing

-- | Prism that constructs cubic bezier segments.  Can also destruct them, if
--   segment is a 'Cubic'.
_bezier3 :: Prism' (Segment Closed v n) (v n, v n, v n)
_bezier3 = prism' (\(c1, c2, c3) -> bezier3 c1 c2 c3) fromBezier3
  where
    fromBezier3 :: Segment c v n -> Maybe (v n, v n, v n)
    fromBezier3 (Cubic c1 c2 (OffsetClosed c3)) = Just (c1, c2, c3)
    fromBezier3 _ = Nothing


-- * Diagrams.Trail

_lineVertices
  :: ( Metric v, OrderedField n
     , Metric v', OrderedField n')
  => Iso
    (Located (Trail' Line v n)) (Located (Trail' Line v' n'))
    [Point v n] [Point v' n']
_lineVertices = iso lineVertices fromVertices

_lineOffsets
  :: ( Metric v, OrderedField n
     , Metric v', OrderedField  n')
  => Iso
    (Trail' Line v n) (Trail' Line v' n')
    [v n] [v' n']
_lineOffsets = iso lineOffsets lineFromOffsets

_lineSegments
  :: (Metric v', OrderedField n')
  => Iso
    (Trail' Line v n) (Trail' Line v' n')
    [Segment Closed v n] [Segment Closed v' n']
_lineSegments = iso lineSegments lineFromSegments
