{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagrams.TwoD.Path.Metafont where

import Control.Lens

import Diagrams.Prelude hiding ((&))

import  Diagrams.TwoD.Path.Metafont.Types

{-

1. [ ] Empty direction @ beginning or end of path -> curl 1. (Implement
       in direction solving code.)  Note cyclic paths have no
       beginning/end; will use cyclic tridiagonal.

2. [ ] Empty direction next to & -> curl 1.  (Should implement in & method.)

3. [ ] empty P nonempty -> replace empty with nonempty.

4. [ ] nonempty P empty -> replace " " " UNLESS nonempty follows explicit control pts.

       i.e. direction after control pts is always ignored.

5. [ ] .. z .. controls u and ...  -> {u - z} z ... controls if (u /=
       z), or {curl 1} if u = z

       Similarly  controls u and v ... z ... ->  z {z - v} (or curl 1)

-}

-- rule 3
copyDirsL :: [MetafontSegment (Maybe PathDir) BasicJoin] -> [MetafontSegment (Maybe PathDir) BasicJoin]
copyDirsL (s1@(MFS _ (PJ _ _ Nothing) _) : segs@(MFS _ (PJ (Just d) _ _) _ : _))
  = (s1 & pj.d2 .~ Just d) : copyDirsL segs
copyDirsL (s1 : segs) = s1 : copyDirsL segs
copyDirsL segs = segs

-- rule 4
copyDirsR :: [MetafontSegment (Maybe PathDir) BasicJoin] -> [MetafontSegment (Maybe PathDir) BasicJoin]
copyDirsR (s1@(MFS _ (PJ _ (Left _) (Just d)) _) : s2@(MFS _ (PJ Nothing _ _) _) : segs)
  = s1 : copyDirsR ((s2 & pj.d1 .~ Just d) : segs)
copyDirsR (s1 : segs) = s1 : copyDirsR segs
copyDirsR segs = segs

-- rule 5
inheritDirs :: [MetafontSegment (Maybe PathDir) BasicJoin] -> [MetafontSegment (Maybe PathDir) BasicJoin]
inheritDirs = undefined

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