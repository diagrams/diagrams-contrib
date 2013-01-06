{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Diagrams.TwoD.Path.Metafont where

import Diagrams.CubicSpline.Internal
import Diagrams.Prelude

data PathJoin d j = PathJoin d j d
  deriving (Functor)

data PathDir
  = PathDirEmpty
  | PathDirCurl Curl
  | PathDirDir  Dir

type Curl = Double
type Dir  = R2

data BasicJoin
  = Simple   -- &
  | Free     -- ..
  | Tense    TensionJoin
  | Ctrl     ControlJoin

data TensionJoin = TensionJoin Tension Tension

data Tension
  = TensionAmt Double
  | TensionAtLeast Double

getTension :: Tension -> Double
getTension (TensionAmt t)     = t
getTension (TensionAtLeast t) = t

data ControlJoin = ControlJoin P2 P2

data MetafontPath = MetafontPath Bool (MFPathData P2)

data MFPathData a where
  MFPathEnd  :: P2 -> MFPathData P2
  MFPathPt   :: P2 -> MFPathData (PathJoin PathDir BasicJoin) -> MFPathData P2
  MFPathJoin :: PathJoin PathDir BasicJoin -> MFPathData P2 -> MFPathData (PathJoin PathDir BasicJoin)

data MetafontSegment d j = MFSegment P2 (PathJoin d j) P2
  deriving (Functor)

-- | Take a segment whose endpoint directions have been fully
--   determined, and compute the control points to realize it as a
--   cubic Bézier segment.  If the segment already has control points
--   specified, the directions are ignored (they are assumed to
--   match).  Otherwise, the segment has tensions specified, and we
--   put the direction and tension data through the magical 'ctrlPts'
--   function to determine the proper control points.  Afterwards we
--   can forget the direction information (since the control points
--   are what we really want, and the directions can be recovered by
--   subtracting the control points from the endpoints anyway).
computeControls
  :: MetafontSegment Dir (Either TensionJoin ControlJoin)
  -> MetafontSegment ()  ControlJoin
computeControls (MFSegment z0 (PathJoin _ (Right cj) _) z1)
  = MFSegment z0 (PathJoin () cj ()) z1
computeControls (MFSegment z0 (PathJoin w0 (Left (TensionJoin a b)) w1) z1)
  = MFSegment z0 (PathJoin () (ControlJoin u v) ()) z1
  where
    (u,v) = ctrlPts z0 w0 (getTension a) (getTension b) w1 z1

-- | Compute the control points for a cubic bezier, given a segment
--   where we know the directions and tensions at both endpoints,
--   i.e. go from
--
--   @z0{w0} .. tension a and b .. {w1}z1@
--
--   to
--
--   @z0 .. controls u and v .. z1@.
--
--   This uses a mysterious, magical formula due to John Hobby.
ctrlPts :: P2 -> R2 -> Double -> Double -> R2 -> P2 -> (P2, P2)
ctrlPts z0 w0 a b w1 z1 = (u,v)
  where
    offs  = z1 .-. z0
    theta = direction w0   - direction offs
    phi   = direction offs - direction w1
    u     = z0 .+^ (offs # rotate theta  # scale (hobbyF theta phi / a))
    v     = z1 .-^ (offs # rotate (-phi) # scale (hobbyF phi theta / b))

-- | Some weird function that computes some sort of scaling factor
--   based on the turning angles between endpoints and direction
--   vectors (again due to Hobby).
hobbyF :: Rad -> Rad -> Double
hobbyF (Rad phi) (Rad theta) =
  (2 + sqrt 2 * (sin theta - sin phi / 16)*(sin phi - sin theta / 16)*(cos theta - cos phi))
  /
  (3 * (1 + (sqrt 5 - 1)/2 * cos theta + (3 - sqrt 5)/2 * cos phi))