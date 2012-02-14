-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Turtle
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
--
-- Stateful domain specific language for diagram paths, modelled after the
-- classic \"turtle\" graphics language.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Path.Turtle
  ( Turtle, runTurtle

    -- * Motion commands
  , forward, backward, left, right

    -- * State accessors / setters
  , heading, setHeading, towards
  , pos, setPos

    -- * Drawing control
  , penHop, penUp, penDown, isDown
  , closeCurrent
  ) where

import Diagrams.Prelude

import qualified Control.Monad.State as ST

type Turtle = ST.State TState

data TState = TState Bool Deg (Path R2)

-- Unexported utilities

-- The path is stored backwards to make accumulation efficient.
-- TODO: consider keeping the output backwards, and always update the position?
-- This would make the "position" query more efficient.
getPath :: TState -> Path R2
getPath (TState d _ (Path xs)) 
  = Path . reverse
  $ map (\(p, (Trail ys c)) -> (p, Trail (reverse ys) c))
  $ if d then xs else tail xs

-- Adds a segment to the accumulated path.
logoseg :: (Segment R2) -> Turtle ()
logoseg seg = ST.modify
  (\(TState d ang p) ->
     TState d ang $ modifyTrail
       (\(Trail xs c) -> Trail (rotate ang seg:xs) c) p)

modifyAngle :: (Deg -> Deg) -> Turtle ()
modifyAngle f = ST.modify (\(TState d a p) -> TState d (f a) p)

modifyPath :: (Path R2 -> Path R2) -> TState -> TState
modifyPath f (TState d ang p) = TState d ang $ f p

modifyTrail :: (Trail v -> Trail v) -> Path v -> Path v
modifyTrail f (Path ((p, t) : ps)) = Path $ (p, f t) : ps
modifyTrail _ p = p


-- | Run the turtle, yielding a path consisting of the traced trails.
runTurtle :: Turtle a -> Path R2
runTurtle t = getPath . snd . ST.runState t 
            $ TState True 0 (Path [(P (0,0), Trail [] False)])

-- Motion commands

-- | Move the turtle forward, along the current heading.
forward :: Double -> Turtle ()
forward  x = logoseg $ Linear (x,          0)

-- | Move the turtle backward, directly away from the current heading.
backward :: Double -> Turtle ()
backward x = logoseg $ Linear ((negate x), 0)

-- | Modify the current heading to the left by the specified angle in degrees.
left :: Double -> Turtle ()
left  a = modifyAngle (+        (Deg a))

-- | Modify the current heading to the right by the specified angle in degrees.
right :: Double -> Turtle ()
right a = modifyAngle (subtract (Deg a))


-- Based on "bezierFromSweepQ1" from Diagrams.TwoD.Arc
{-
smoothTurn f s = 
  where (x,y) = rotate s (1, 0)
        (u,v) = ((4-x)/3, (1-x)*(3-x)/(3*y))

bezierFromSweepQ1 :: Rad -> Segment R2
bezierFromSweepQ1 s = fmap (^-^ v) . rotate (s/2) $ Cubic p2 p1 p0
        p2       = reflectY p1
-}

-- State accessors / setters

-- | Set the current turtle angle, in degrees.
setHeading :: Double -> Turtle ()
setHeading a = modifyAngle (const (Deg a))

-- | Get the current turtle angle, in degrees.
heading :: Turtle Double
heading = ST.gets (\(TState _ (Deg x) _) -> x)

-- | Sets the heading towards a given location.
towards :: R2 -> Turtle ()
towards pt = do
  p <- pos
  setHeading . uncurry atan2 $ pt ^-^ p

-- | Set the current turtle X/Y position.
setPos :: R2 -> Turtle ()
setPos p = ST.modify helper
 where
  helper (TState d a (Path ps))
    = TState d a $ Path $ (P p, Trail [] False)
                             : if d then ps else tail ps

-- | Get the current turtle X/Y position.
pos :: Turtle R2
pos = ST.gets 
  (\(TState _ _ (Path ((P p, t) : _))) -> p ^+^ trailOffset t)

-- Drawing control.

-- | Starts a new path at the current location.
penHop :: Turtle ()
penHop = pos >>= setPos

-- | Ends the current path, and enters into "penUp" mode
penUp :: Turtle ()
penUp   = penHop >> ST.modify (\(TState _ a p) -> TState False a p)

-- | Ends the current path, and enters into "penDown" mode
penDown :: Turtle ()
penDown = penHop >> ST.modify (\(TState _ a p) -> TState True a p)

-- | Queries whether the pen is currently drawing a path or not.
isDown :: Turtle Bool
isDown = ST.gets (\(TState d _ _) -> d)

-- | Closes the current path, to the last penDown / setPosition
-- Maintains current position - does this make sense?
closeCurrent :: Turtle ()
closeCurrent = do
  p <- pos
  ST.modify $ modifyPath $ modifyTrail close
  setPos p
