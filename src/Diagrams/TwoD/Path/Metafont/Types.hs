{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagrams.TwoD.Path.Metafont.Types where

import Control.Lens hiding ((#))
import Data.Monoid
import Data.Semigroup

import Diagrams.TwoD.Types

-- | A @PathJoin@ specifies the directions at both ends of a segment,
-- and a join which describes the control points explicitly or implicitly.
data PathJoin d j = PJ { _d1 :: d, _j :: j, _d2 :: d }
  deriving (Functor, Show)

makeLenses ''PathJoin

-- | A direction can be specified at any point of a path.  A /curl/
-- should only be specified at the endpoints.  The endpoints default
-- to curl 1 if not set.
data PathDir
  = PathDirCurl Curl
  | PathDirDir  Dir
    deriving Show

-- | A predicate to determine the constructor used.
isCurl :: PathDir -> Bool
isCurl (PathDirDir _) = False
isCurl (PathDirCurl _) = True

type Curl = Double
type Dir  = R2

type BasicJoin = Either TensionJoin ControlJoin

-- | Higher /Tension/ brings the path closer to a straight line
-- between segments.  Equivalently, it brings the control points
-- closer to the endpoints.  @TensionAmt@ introduces a fixed tension.
-- @TensionAtLeast@ introduces a tension which will be increased if by
-- so doing, an inflection point can be eliminated.
data Tension
  = TensionAmt Double
  | TensionAtLeast Double
  deriving Show

getTension :: Tension -> Double
getTension (TensionAmt t)     = t
getTension (TensionAtLeast t) = t

-- | Two tensions and two directions completely determine the control
-- points of a segment.
data TensionJoin = TJ { _t1 :: Tension, _t2 :: Tension }
                 deriving Show

-- | The two intermediate control points of a segment, specified directly.
data ControlJoin = CJ { _c1 :: P2, _c2 :: P2 }
                 deriving Show

makeLenses ''TensionJoin
makeLenses ''ControlJoin

data P
data J

-- | @MFPathData@ is the type manipulated by the metafont combinators.
data MFPathData a where
  MFPathCycle:: MFPathData P
  MFPathEnd  :: P2 -> MFPathData P
  MFPathPt   :: P2 -> MFPathData J -> MFPathData P
  MFPathJoin :: PathJoin (Maybe PathDir) (Maybe BasicJoin) -> MFPathData P -> MFPathData J

-- | @MetafontSegment@ is used internally in solving the metafont
-- equations.  It represents a segment with two known endpoints, and a
-- /join/, which may be specified in various ways.
data MetafontSegment d j = MFS { _x1 :: P2, _pj :: (PathJoin d j), _x2 :: P2 }
                         deriving (Functor, Show)

-- | @MFPath@ is the type used internally in solving the metafont
-- equations.  The direction and join types are progressively refined
-- until all control points are known.  The @loop@ flag affects both
-- the equations to be solved and the type of 'Trail' in the result.
-- If constructing an @MFPath@ in new code, the responsibility rests
-- on the user to ensure that successive @MetafontSegment@s share an
-- endpoint.  If this is not true, the result is undefined.
data MFPath d j = MFP { _loop :: Bool, _segs :: [MetafontSegment d j] }
                deriving Show

-- | MFP is a type synonym to clarify signatures in Metafont.Internal.
-- Note that the type permits segments which are \"overspecified\",
-- having one or both directions specified, and also a 'ControlJoin'.
-- In this case, "Metafont.Internal" ignores the directions.
type MFP = MFPath (Maybe PathDir) BasicJoin

-- | MFS is a type synonym to clarify signatures in "Metafont.Internal".
type MFS = MetafontSegment (Maybe PathDir) BasicJoin

makeLenses ''MetafontSegment
makeLenses ''MFPath

instance Monoid (PathJoin (Maybe PathDir) (Maybe BasicJoin)) where
    -- | The default join, with no directions specified, and both tensions 1.
    mempty = PJ Nothing Nothing Nothing
    l `mappend` r = PJ (c (l^.d1) (r^.d1)) (c (l^.j) (r^.j)) (c (l^.d2) (r^.d2))
      where
        c a b = case b of
            Nothing -> a
            Just _  -> b

instance Semigroup (PathJoin (Maybe PathDir) (Maybe BasicJoin)) where
    (<>) = mappend

