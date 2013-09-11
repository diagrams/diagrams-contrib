{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagrams.TwoD.Path.Metafont.Internal where

import Control.Lens hiding ((#), at)
import Data.Maybe

import Diagrams.Prelude hiding ((&), view, over)

import  Diagrams.TwoD.Path.Metafont.Types


-- | Reverse a MetaFont segment, including all directions & joins.
reverseSeg :: MFS -> MFS
reverseSeg s = MFS (s^.x2) (PJ (rDir $ s^.pj.d2) (s^.pj.j.to rj) (rDir $ s^.pj.d1)) (s^.x1) where
  rj (Left t) = (Left (TJ (t^.t2) (t^.t1)))
  rj (Right c) = (Right (CJ (c^.c2) (c^.c1)))
  rDir (Just (PathDirDir d)) = (Just (PathDirDir (negateV d)))
  rDir d = d

-- | Calculate the length of a MetaFont segment.
mfSegmentLength :: MetafontSegment p j -> Double
mfSegmentLength = magnitude . mfSegmentOffset

-- | Calculate the vector between endpoints of the given segment.
mfSegmentOffset :: MetafontSegment p j -> R2
mfSegmentOffset s = s^.x2 .-. s^.x1

-- | leftCurl s is True if the first direction of s is specified as a curl
leftCurl, rightCurl :: MFS -> Bool
leftCurl (MFS _ (PJ (Just (PathDirCurl _)) _ _) _) = True
leftCurl _ = False

-- | rightCurl s is True if the second direction of s is specified as a curl
rightCurl (MFS _ (PJ _ _ (Just (PathDirCurl _))) _) = True
rightCurl _ = False

-- | Normalize a number representing number of turns to ±½
normalizeTurns :: Double -> Double
normalizeTurns t | t >  1/2   = t - realToFrac (ceiling t)
normalizeTurns t | t < -1/2   = t - realToFrac (floor t)
normalizeTurns t = t

-- | By analogy with fromJust, fromLeft returns the Left value or errors
fromLeft :: Either a b -> a
fromLeft (Left j) = j
fromLeft (Right _) = error "got Right in fromLeft"

{-

fillDirs implements all of the following rules:

1. [ ] Empty direction @ beginning or end of path -> curl 1. 
       Note cyclic paths have no beginning/end; will use cyclic tridiagonal.

2. [ ] Empty direction next to & -> curl 1.

3. [ ] empty P nonempty -> replace empty with nonempty.

4. [ ] nonempty P empty -> replace " " " UNLESS nonempty follows explicit control pts.

       i.e. direction after control pts is always ignored.

5. [ ] .. z .. controls u and ...  -> {u - z} z ... controls if (u /=
       z), or {curl 1} if u = z

       Similarly  controls u and v ... z ... ->  z {z - v} (or curl 1)

-}

-- rules 1 & 2
curlEnds :: MFP -> MFP
curlEnds p@(MFP True _) = p
curlEnds (MFP False ss) = MFP False $ end ss where
  end  [s]      = [s & pj.d1 %~ curlIfEmpty & pj.d2 %~ curlIfEmpty]
  end  (s:segs) = (s & pj.d1 %~ curlIfEmpty) : end' segs
  end' []       = []
  end' (s:[])   = (s & pj.d2 %~ curlIfEmpty) : []
  end' (s:segs) = s:end' segs
  curlIfEmpty Nothing = Just $ PathDirCurl 1
  curlIfEmpty d = d

-- rule 3
copyDirsL :: [MFS] -> [MFS]
copyDirsL (s1@(MFS _ (PJ _ _ Nothing) _) : segs@(MFS _ (PJ (Just d) _ _) _ : _))
  = (s1 & pj.d2 .~ Just d) : copyDirsL segs
copyDirsL (s1 : segs) = s1 : copyDirsL segs
copyDirsL [] = []

-- rule 4
copyDirsR :: [MFS] -> [MFS]
copyDirsR (s1@(MFS _ (PJ _ (Left _) (Just d)) _) : s2@(MFS _ (PJ Nothing _ _) _) : segs)
  = s1 : copyDirsR ((s2 & pj.d1 .~ Just d) : segs)
copyDirsR (s1 : segs) = s1 : copyDirsR segs
copyDirsR [] = []

-- copy a direction from one end of a loop to the other
copyDirsLoop :: MFP -> MFP
copyDirsLoop p | not $ _loop p = p
copyDirsLoop p@(MFP _ []) = p
copyDirsLoop p | (p^?!segs._head.pj.d1.to isJust) &&
                 (p^?!segs._last.pj.d2.to isNothing) =
                   p & over (segs._last.pj.d2) (const $ p^?!segs._head.pj.d1)
copyDirsLoop p | p^?!segs._head.pj.d1.to isNothing &&
                 p^?!segs._last.pj.d2.to isJust =
                   p & over (segs._head.pj.d1) (const $ p^?!segs._last.pj.d2)
copyDirsLoop p = p

-- rule 5
-- apply rule 5 before rules 3 & 4, then depend on those rules to copy the directions
-- into adjacent segments
controlPtDirs :: MFS -> MFS
controlPtDirs s@(MFS z0 (PJ _ (Right (CJ u v)) _) z1) = s & pj %~ dirs where
  dirs :: PathJoin (Maybe PathDir) j -> PathJoin (Maybe PathDir) j
  dirs (PJ _ j _) = PJ (dir z0 u) j (dir v z1)
  dir :: P2 -> P2 -> Maybe PathDir
  dir p0 p1 | p0 == p1 = Just $ PathDirCurl 1
  dir p0 p1 | otherwise = Just $ PathDirDir (p1 .-. p0)
controlPtDirs s = s

-- | Fill in default values for as many blank directions as possible.
fillDirs :: MFP -> MFP
fillDirs p  = (copyDirsLoop . curlEnds) p & segs %~
              (copyDirsR . copyDirsL . map controlPtDirs)

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
computeControls (MFS z0 (PJ _ (Right cj) _) z1)
  = MFS z0 (PJ () cj ()) z1
computeControls (MFS z0 (PJ w0 (Left (TJ a b)) w1) z1)
  = MFS z0 (PJ () (CJ u v) ()) z1
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

-- These are potentially useful for a combinator aproach to defining MF paths.
--  More design work is needed.
mfPathToSegments :: MFPathData P -> [MFS]
mfPathToSegments = snd . mfPathToSegments'
  where
    mfPathToSegments' :: MFPathData P -> (P2, [MFS])
    mfPathToSegments' (MFPathEnd p0) = (p0, [])
    mfPathToSegments' (MFPathPt p0 (MFPathJoin j path)) = (p0, MFS p0 j p1 : segs)
      where
        (p1, segs) = mfPathToSegments' path
