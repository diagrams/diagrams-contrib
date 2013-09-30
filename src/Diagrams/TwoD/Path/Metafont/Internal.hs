{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagrams.TwoD.Path.Metafont.Internal where

import Control.Lens hiding ((#), at)
import Data.Maybe

import Diagrams.CubicSpline.Internal
import Diagrams.Prelude hiding (view, over)

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
normalizeTurns t | t >  1/2   = t - realToFrac (ceiling t :: Int)
normalizeTurns t | t < -1/2   = t - realToFrac (floor t   :: Int)
normalizeTurns t = t

-- | By analogy with fromJust, fromLeft returns the Left value or errors
fromLeft :: Either a b -> a
fromLeft (Left l) = l
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
curlEnds (MFP False ss') = MFP False $ leftEnd ss' where
  leftEnd  [s]      = [s & pj.d1 %~ curlIfEmpty & pj.d2 %~ curlIfEmpty]
  leftEnd  (s:ss)   = (s & pj.d1 %~ curlIfEmpty) : rightEnd ss
  leftEnd  []       = []
  rightEnd []       = []
  rightEnd (s:[])   = (s & pj.d2 %~ curlIfEmpty) : []
  rightEnd (s:ss)   = s:rightEnd ss
  curlIfEmpty Nothing = Just $ PathDirCurl 1
  curlIfEmpty d       = d

-- rule 3
copyDirsL :: [MFS] -> [MFS]
copyDirsL (s1@(MFS _ (PJ _ _ Nothing) _) : ss@(MFS _ (PJ (Just d) _ _) _ : _))
  = (s1 & pj.d2 .~ Just d) : copyDirsL ss
copyDirsL (s1 : ss') = s1 : copyDirsL ss'
copyDirsL [] = []

-- rule 4
copyDirsR :: [MFS] -> [MFS]
copyDirsR (s1@(MFS _ (PJ _ (Left _) (Just d)) _) : s2@(MFS _ (PJ Nothing _ _) _) : ss)
  = s1 : copyDirsR ((s2 & pj.d1 .~ Just d) : ss)
copyDirsR (s1 : ss') = s1 : copyDirsR ss'
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
  dirs (PJ _ jj _) = PJ (dir z0 u) jj (dir v z1)
  dir :: P2 -> P2 -> Maybe PathDir
  dir p0 p1 | p0 == p1 = Just $ PathDirCurl 1
  dir p0 p1 | otherwise = Just $ PathDirDir (p1 .-. p0)
controlPtDirs s = s

-- | Fill in default values for as many blank directions as possible.
fillDirs :: MFP -> MFP
fillDirs p  = (copyDirsLoop . curlEnds) p & segs %~
              (copyDirsR . copyDirsL . map controlPtDirs)

-- | Run all the rules required to fully specify all segment directions,
-- but do not replace the Joins with ControlJoin.
solve :: MFP -> MFPath Dir BasicJoin
solve = solvePath . fillDirs

-- | each sublist of groupSegments ss satisfies:
-- isJust . d1 . pj . head
-- isJust . d2 . pj . last
-- all (isNothing . d1 . pj) . init . tail
-- all (isNothing . d2 . pj) . init . tail
-- That is, each sublist can be handled as a line,
-- (except the first and last, if the initial MFP was a loop).
groupSegments :: [MFS] -> [[MFS]]
groupSegments [] = []
groupSegments (s:ss) = (s:open):groupSegments rest where
  (open,rest) = span (view $ pj.d1.to isNothing) ss

-- | Calculate the tangent direction at all remaining points.
-- This function dispatches all of the hard work to other functions.
-- It distinguishes 3 cases:
-- * A loop with no internal directions given.
-- * A loop with one or more directions given.
--     Mathematically, this is handled like a line, but the Loopness is
--     preserved, so that the Diagrams Trail is a Loop.
-- * A line, consisting of one or more segments as described in groupSegments.
-- Note that the result type is different from the input, reflecting
-- fully specified directions.
solvePath :: MFP -> MFPath Dir BasicJoin
solvePath (MFP False ss) = MFP False (concat . map solveLine . groupSegments $ ss)
-- A simple loop.  All directions are unknown, curvature gives us enough equations.
solvePath (MFP True ss) | all (view $ pj.d1.to isNothing) ss = MFP True $ solveLoop ss
solvePath (MFP True ss) = MFP True ss'' where
  ss' = groupSegments ss
  ss'' = concat . map solveLine $ case ss'^?!_head^?!_head.pj.d1 of
      (Just (PathDirDir _)) -> ss'
      _ -> (init . tail $ ss') ++ [last ss' ++ head ss']

-- | Calculate the tangent directions at all points.  The input list is assumed
-- to form a loop; this is not checked.
-- See setDirs for an explanation of offset angles.
solveLoop :: [MFS] -> [MetafontSegment Dir BasicJoin]
solveLoop ss = zipWith3 setDirs ss thetas phis where
  segmentPairs = zip ss (tail . cycle $ ss)
  thetas = loopDirs ss
  phis = map negate $ zipWith (+) (map psi segmentPairs) (tail . cycle $ thetas)

-- | Calculate the offset angles θ for the case of a loop.
--   This is a system of (length ss) equations.  The first element of
--   loopDirs ss is θ for the starting point of the first segment of ss.
loopDirs :: [MFS] -> [Double]
loopDirs ss = solveCyclicTriDiagonal lower diag upper products ll ur where
  (lower, diag, upper, products, ll, ur) = loopEqs ss

-- | Calculate the coefficients for the loop case, in the
-- format required by solveCyclicTriDiagonal.
-- See mf.web ¶ 273
loopEqs :: [MFS]
           -> ([Double], [Double], [Double], [Double], Double, Double)
loopEqs ss = (lower, diag, upper, products, ll, ur) where
  lower = map aCo (init ss)
  sLast = last ss
  diag = zipWith (+) (map bCo $ [sLast] ++ ss) (map cCo ss)
  upper = map dCo (init ss)
  ur = aCo sLast
  ll = dCo sLast
  segmentPairs = zip ([last ss] ++ init ss) ss
  products = zipWith (-)
               [-1 * bCo l * psi s | s@(l,_) <- segmentPairs]
               (zipWith (*)
                (map dCo ss)
                (map psi $ tail segmentPairs)
                ++ [dCo sLast * psi (head segmentPairs)])

-- | solveLine takes a list of segments where only the first and last points
-- have known directions.  The type signature matches that of solveLoop, and the
-- precondition is not checked.
-- The equivalent MetaFont code (in make_choices) is written in terms of points,
-- rather than segments.  See metafont code paragraphs 271--274.
solveLine :: [MFS] -> [MetafontSegment Dir BasicJoin]
solveLine [MFS z1 (PJ (Just (PathDirDir d1')) jj (Just (PathDirDir d2'))) z2] =
  [MFS z1 (PJ d1' jj d2') z2]
solveLine ss = zipWith3 setDirs ss (init thetas) phis where
  segmentPairs = zip (init ss) (tail ss)
  thetas = lineDirs ss
  phis :: [Double]
  phis = map negate $ zipWith (+) (map psi segmentPairs ++ [0]) (tail thetas)

-- | setDirs takes a segment with underspecified directions, and two offset
-- angles, and sets the directions at both ends of the segment.
-- The offset angle is measured between the direction vector at either end and
-- the vector difference of the segment endpoints.
setDirs :: MFS -- ^ The segment to be modified
        -> Double -- ^ theta, the offset angle at the starting point
        -> Double -- ^ phi, the ofset angle at the endpoint
        -> MetafontSegment Dir BasicJoin
setDirs (MFS z0 (PJ w0' jj w1') z1) t p = MFS z0 (PJ w0 jj w1) z1 where
    offs  = z1 .-. z0
    w0 = case w0' of
      (Just (PathDirDir d)) -> d
      _ -> offs # rotate (Turn t)
    w1 = case w1' of
      (Just (PathDirDir d)) -> d
      _ -> offs # rotate (Turn (negate p))

-- | psi (l,r) calculates the turning angle between segments l and r, if
-- each segment were a straight line connecting its endpoints.  The endpoint of l
-- is assumed to be the starting point of r; this is not checked.
psi :: (MetafontSegment p j1, MetafontSegment p j1) -> Double
psi (l,r) = normalizeTurns t where
  (Turn t) =  direction (mfSegmentOffset r) - direction (mfSegmentOffset l)

-- | lineDirs calculates the offset angles θ for a Line.  Most of the work
-- done by lineEqs and solveTriDiagonal, but lineDirs handles the separate cases
-- of an empty list, and lists of length one.  See mf.web ¶ 280.
lineDirs :: [MFS] -> [Double]
lineDirs ss | length ss > 1 = solveTriDiagonal lower diag upper products where
  (lower, diag, upper, products) = lineEqs ss
lineDirs [] = []
lineDirs [s] | leftCurl s && rightCurl s = [0, 1/2] where
lineDirs [s] | rightCurl s = solveTriDiagonal [a] [1,c] [0] [normalizeTurns t, r] where
  (a,c,r) = solveOneSeg s
  (PathDirDir dir) = s^.pj.d1.to fromJust
  (Turn t) = direction dir - direction (s^.x2 .-. s^.x1)
lineDirs [s] | leftCurl s = reverse $ lineDirs [reverseSeg s]
lineDirs s = error $ "lineDirs was called on something inappropriate.  \
\It should be called on a list of segments with directions specified at both ends.\
\It should only be called through solveLine.  The problem was: "++ show s

-- | Each intermediate point produces one curvature equation, as in loopEqs.
-- The endpoint equations are the same as those for the single-segment line in
-- lineDirs.
-- lineEqs only works when segs has length > 1; this precondition is not checked.
lineEqs :: [MFS] -> ([Double], [Double], [Double], [Double])
lineEqs ss = (lower, diag, upper, products) where
  segmentPairs = zip (init ss) (tail ss)
  lower = map aCo (init ss) ++ [an]
  diag  = c0 : zipWith (+) (map bCo (init ss)) (map cCo (tail ss)) ++ [cn]
  upper = (d0 : map dCo (tail ss))
  products = r0 : zipWith (-)
               [-1 * bCo l * psi s | s@(l,_) <- segmentPairs]
               (zipWith (*)
                 (map dCo (tail $ ss))
                 (map psi (tail segmentPairs)
                ++ [0])) ++ [rn]
  (d0,c0,_) = solveOneSeg . reverseSeg $ s0
  r0 = r0' (s0^.pj.d1.to fromJust) where
    r0' (PathDirDir d) = normalizeTurns t where
                         (Turn t) = direction d - direction (s0^.x2 .-. s0^.x1)
    r0' (PathDirCurl _) = negate $ d0 * psi (s0, ss!!1)
  s0 = head ss
  (an, cn, rn) = solveOneSeg (last ss)

-- These functions calculate the coefficients in lineEqs, loopEqs
-- They are derived in mf.web ¶ 272-273
alpha, beta, aCo, bCo, cCo, dCo :: MFS -> Double
alpha s = 1 / s^.pj.j.to fromLeft.t1.to getTension
beta  s = 1 / s^.pj.j.to fromLeft.t2.to getTension
aCo s = (alpha s) / (beta s **2 * mfSegmentLength s)
bCo s = (3 - alpha s) / (beta s **2 * mfSegmentLength s)
cCo s = (3 - beta s) / (alpha s **2 * mfSegmentLength s)
dCo s = (beta s) / (alpha s **2 * mfSegmentLength s)

-- | solveOneSeg calculates the coefficients for the final segment of a line,
-- which may incidentally be the only segment.
solveOneSeg :: MFS -> (Double, Double, Double)
solveOneSeg s = (a, c, r) where
  a = a' (s^.pj.d2.to fromJust) where
    a' (PathDirDir _) = 0
    a' (PathDirCurl g) = (3 - beta s) * (beta s) **2 * g / (alpha s **2) + alpha s
  c = c' (s^.pj.d2.to fromJust) where
       c' (PathDirDir _) = 1
       c' (PathDirCurl g) = beta s **3 * g / (alpha s **2) + 3 - alpha s
  r = r' (s^.pj.d2.to fromJust) where
    r' (PathDirDir d) = normalizeTurns t where
      (Turn t) = direction d - direction (s^.x2 .-. s^.x1)
    r' (PathDirCurl _) = 0

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

-- | Convert a fully specified MetafontSegment to a Diagrams Segment
importSegment :: MetafontSegment () ControlJoin -> Segment Closed R2
importSegment (MFS z0 (PJ () (CJ u v) ()) z1) = bezier3 (u .-. z0) (v .-. z0) (z1 .-. z0)

-- | Convert a MetaFont path to a Diagrams Trail, using a Loop or Line as needed
locatedTrail :: MFPath () ControlJoin -> Located (Trail R2)
locatedTrail (MFP False ss)  = (wrapLine . fromSegments . map importSegment $ ss)
                                `at` (head ss ^.x1)
locatedTrail (MFP True ss)   = (wrapLoop . fromSegments . map importSegment $ ss)
                                `at` (head ss ^.x1)

-- These are potentially useful for a combinator aproach to defining MF paths.
--  More design work is needed.
mfPathToSegments :: MFPathData P -> MFP
mfPathToSegments = fixCycleSegment . snd . mfPathToSegments'
  where
    mfPathToSegments' :: MFPathData P -> (P2, MFP)
    mfPathToSegments' (MFPathEnd p0) = (p0, MFP False [])
    mfPathToSegments' MFPathCycle    = (origin, MFP True [])
    mfPathToSegments' (MFPathPt p0 (MFPathJoin jj path)) = (p0, MFP c (MFS p0 jj p1 : ss))
      where
        (p1, MFP c ss) = mfPathToSegments' path
    fixCycleSegment (MFP True ss) = MFP True (ss & _last.x2 .~ ss^?!_head.x1)
    fixCycleSegment p = p
