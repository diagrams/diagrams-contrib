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
-- This module provides utilities for using "Control.Lens" with diagrams.
module Diagrams.Lens
  (
  -- * Diagrams.BoundingBox
    _corners
  -- * Diagrams.Core.Types
  , _location
  -- * Diagrams.Located
  , _Loc
  -- * Diagrams.Parametric
  -- , _arcLength
  -- * Diagrams.Segment
  , _mkFixedSeg
  , _straight
  , _bezier3
  -- * Diagrams.Trail
  , _lineSegments
  ) where

import           Diagrams.BoundingBox
import           Diagrams.Prelude


-- * Diagrams.BoundingBox

-- | A traversal that either has 0 (empty box) or 2 points.  These points are
--   the lower and upper corners, respectively.
_corners
    :: (Additive v', Foldable v', Ord n')
       => Traversal (BoundingBox v n) (BoundingBox v' n') (Point v n) (Point v' n')
_corners f (getCorners -> Just (l, t)) = fromCorners <$> f l <*> f t
_corners _ _ = pure emptyBox

-- * Diagrams.Core.Types

-- | Gets or set the 'location' of a 'Subdiagram'.
_location
  :: (HasLinearMap v, Metric v, OrderedField n)
  => Lens' (Subdiagram b v n m) (Point v n)
--TODO: Is this correct??
_location = lens location (flip Diagrams.Prelude.moveTo)

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

_lineSegments
  :: (Metric v', OrderedField n')
  => Iso
    (Trail' Line v n) (Trail' Line v' n')
    [Segment Closed v n] [Segment Closed v' n']
_lineSegments = iso lineSegments lineFromSegments
