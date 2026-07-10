{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Turtle
-- Copyright   :  (c) 2011 Michael Sloan
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Michael Sloan <mgsloan at gmail>,  Deepak Jois <deepak.jois@gmail.com>
-- Authors     :  Michael Sloan <mgsloan at gmail>, Deepak Jois <deepak.jois@gmail.com>
--
-- A module consisting of core types and functions to represent and operate on
-- a \"turtle\".
--
-- More info about turtle graphics:
-- <http://en.wikipedia.org/wiki/Turtle_graphics>
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.Turtle.Internal
  (
    -- * Turtle data types and accessors
    TurtleState(..), TurtlePath(..), PenStyle(..)

    -- * Motion commands
  , forward, backward, left, right

    -- * Pen style commands
  , setPenColor, setPenColour, setPenWidth

    -- * State setters
  , startTurtle, setHeading, towards
  , setPenPos

    -- * Drawing control
  , penUp, penDown, penHop, closeCurrent

    -- * Diagram related
  , getTurtleDiagram
  , getTurtlePath
  ) where

import           Diagrams.Prelude

-- | Style attributes associated with the turtle pen
data PenStyle n = PenStyle
  { penWidth :: Measure n -- ^ Width of pen. Default is 1.0
  , penColor :: Colour Double  -- ^ Color of pen. Default is @black@
  }

-- | Turtle path type that captures a list of paths and the style attributes
-- associated with them
data TurtlePath n = TurtlePath
  { penStyle    :: PenStyle n            -- ^ Style
  , turtleTrail :: Located (Trail V2 n)  -- ^ Path
  }

-- | Core turtle data type. A turtle needs to keep track of its current
-- position, like its position, heading etc., and all the paths that it has
-- traversed so far.
--
-- We need to record a new path, everytime an attribute like style, pen position
-- etc changes, so that we can separately track styles for each portion of the
-- eventual path that the turtle took.
data TurtleState n = TurtleState
  { -- | State of the pen. @False@ means that turtle movements will not draw
    -- anything
    isPenDown    :: Bool
     -- | Current position. This is updated everytime the turtle moves
  , penPos       :: P2 n
     -- | Orientation of the turtle in 2D space, given in degrees
  , heading      :: Angle n
     -- | Path traversed by the turtle so far, without any style or pen
     -- attributes changing
  , currTrail    :: Located (Line V2 n)
     -- | Current style of the pen
  , currPenStyle :: PenStyle n
     -- | List of paths along with style information, traversed by the turtle
     -- previously
  , paths        :: [TurtlePath n]
  }

-- | Default pen style, with @penWidth@ set to 1.0 and @penColor@ set to black
defaultPenStyle :: (Floating n, Ord n) => PenStyle n
defaultPenStyle = PenStyle (normalized 0.004 `atLeast` output 0.5) black

-- | The initial state of turtle. The turtle is located at the origin, at an
-- orientation of 0 degrees with its pen position down. The pen style is
-- @defaultPenStyle@.
startTurtle :: (Floating n, Ord n) => TurtleState n
startTurtle = TurtleState True origin zero (mempty `at` origin) defaultPenStyle []

-- | Draw a segment along the turtle’s path and update its position. If the pen
-- is up, only the position is updated.
moveTurtle :: (Floating n, Ord n) => Segment V2 n -- ^ Segment representing the path to travel
           -> TurtleState n       -- ^ Turtle to move
           -> TurtleState n       -- ^ Resulting turtle
moveTurtle s t@(TurtleState pd pos h tr _ _) =
 if pd
   -- Add segment to current trail and update position
   then t { currTrail = newTrail
          , penPos = newPenPos
          }
   -- Update position only
   else t { penPos = newPenPos }
 where
   -- Rotate segment by orientation before adding to trail
   rotatedSeg  =  rotate h s
   newTrail    =  mapLoc (<> fromSegments [rotatedSeg]) tr
   -- Calculate the new position along the segment
   newPenPos   =  pos .+^ offset rotatedSeg

-- | Move the turtle forward by @x@ units
forward :: (Floating n, Ord n) => n -- ^ Distance to move
        -> TurtleState n  -- ^ Turtle to move
        -> TurtleState n -- ^ Resulting turtle
forward x = moveTurtle (straight $ r2 (x,0))

-- | Move the turtle backward by @x@ units
backward :: (Floating n, Ord n) => n -- ^ Distance to move
         -> TurtleState n -- ^ Turtle to move
         -> TurtleState n -- ^ Resulting turtle
backward x = moveTurtle (straight $ r2 (negate x, 0))

-- | Turn the turtle by applying the given function to its current orientation
-- (in degrees)
turnTurtle :: (Angle n -> Angle n)   -- ^ Transformation to apply on current orientation
           -> TurtleState n    -- ^ Turtle to turn
           -> TurtleState n    -- ^ Resulting turtle
turnTurtle f t@(TurtleState _ _ h _ _ _) = t { heading = f h  }

-- | Turn the turtle anti-clockwise (left)
left :: Floating n
     => n              -- ^ Degree of turn
     -> TurtleState n  -- ^ Turtle to turn
     -> TurtleState n  -- ^ Resulting turtle
left d = turnTurtle (^+^ (d @@ deg))

-- | Turn the turtle clockwise (right)
right :: Floating n
      => n              -- ^ Degree of turn
      -> TurtleState n  -- ^ Turtle to turn
      -> TurtleState n  -- ^ Resulting turtle
right d = turnTurtle (^-^ (d @@ deg))

-- | Turn the turtle to the given orientation, in degrees
setHeading :: Floating n
           => n              -- ^ Degree of orientation
           -> TurtleState n  -- ^ Turtle to orient
           -> TurtleState n  -- ^ Resulting turtle
setHeading d = turnTurtle (const $ d @@ deg)

-- | Sets the turtle orientation towards a given location.
towards :: RealFloat n =>
           P2 n           -- ^ Point to orient turtle towards
        -> TurtleState n  -- ^ Turtle to orient
        -> TurtleState n  -- ^ Resulting turtle
towards p  = setHeading =<< (360 *) . (/ tau) . uncurry atan2 . unr2 . (p .-.) . penPos

-- | Puts the turtle pen in “Up” mode. Turtle movements will not draw anything
--
-- Does nothing if the pen was already up. Otherwise, it creates a turtle with
-- the current trail added to @paths@.
penUp :: (Ord n, Floating n) => TurtleState n  -- ^ Turtle to modify
      -> TurtleState n  -- ^ Resulting turtle
penUp t
 | isPenDown t = t # makeNewTrail #  \t' -> t' { isPenDown = False }
 | otherwise   = t

-- | Puts the turtle pen in “Down” mode. Turtle movements will cause drawing to
-- happen
--
-- Does nothing if the pen was already down. Otherwise, starts a new trail
-- starting at the current position.
penDown :: (Ord n, Floating n) => TurtleState n  -- ^ Turtle to modify
        -> TurtleState n  -- ^ Resulting turtle
penDown t
  | isPenDown t = t
  | otherwise   = t # makeNewTrail #  \t' -> t' { isPenDown = True }

-- Start a new trail at current position
penHop :: (Ord n, Floating n) => TurtleState n
       -> TurtleState n
penHop t = t # makeNewTrail

-- Closes the current path, to the starting position of the current
-- trail. Has no effect when the pen is up.
closeCurrent :: (Floating n, Ord n) => TurtleState n
             -> TurtleState n
closeCurrent t
  | isPenDown t = t # closeTTrail
  | otherwise   = t
 where startPos = loc . currTrail $ t
       closeTTrail t'  = t' { penPos    = startPos
                            , currTrail = mempty `at` startPos
                            , paths     = addTrailToPath t'
                                            (mapLoc (wrapLoop . closeLine) $ currTrail t)
                            }

-- | Set the turtle X/Y position.
--
-- If pen is down and the current trail is non-empty, this will also add the
-- current trail to the @paths@ field.
setPenPos :: (Ord n, Floating n) => P2 n           -- ^ Position to place true
          -> TurtleState n  -- ^ Turtle to position
          -> TurtleState n  -- ^ Resulting turtle
setPenPos newPos t = t {penPos = newPos } # makeNewTrail

-- | Set a new pen width for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenWidth :: (Ord n, Floating n) => Measure n -- ^ Width of Pen
            -> TurtleState n  -- ^ Turtle to change
            -> TurtleState n  -- ^ Resulting Turtle
setPenWidth w = modifyCurrStyle (\s -> s { penWidth = w })
-- | Set a new pen color for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenColour :: (Ord n, Floating n) => Colour Double  -- ^ Width of Pen
             -> TurtleState n    -- ^ Turtle to change
             -> TurtleState n    -- ^ Resulting Turtle
setPenColour c = modifyCurrStyle (\s -> s { penColor = c })

-- | alias of @setPenColour@
setPenColor ::  (Ord n, Floating n) => Colour Double  -- ^ Width of Pen
            -> TurtleState n    -- ^ Turtle to change
            -> TurtleState n    -- ^ Resulting Turtle
setPenColor = setPenColour

-- | Creates a diagram from a turtle
--
-- Applies the styles to each trails in @paths@ separately and combines them
-- into a single diagram
getTurtleDiagram :: TurtleState Double -> Diagram V2
getTurtleDiagram t =
  mconcat .
  map turtlePathToStroke .
  paths $ t # penUp -- Do a penUp to add @currTrail@ to @paths@

-- | Creates a path from a turtle, ignoring the styles.
getTurtlePath :: (Floating n, Ord n) => TurtleState n -> Path V2 n
getTurtlePath = mconcat . map turtlePathToTrailLike . paths . penUp

-- * Helper functions

-- Makes a "TurtlePath" from a "Turtle"’s @currTrail@ field
makeTurtlePath :: TurtleState n
               -> Located (Trail V2 n)
               -> TurtlePath n
makeTurtlePath t tr = TurtlePath (currPenStyle t) tr

-- Returns a list of paths, with current trail added to a "Turtle"’s @paths@ field
addTrailToPath :: (Ord n, Floating n) => TurtleState n
               -> Located (Trail V2 n)
               -> [TurtlePath n]
addTrailToPath t tr
  | has _Empty (unLoc tr) = paths t
  | otherwise            = makeTurtlePath t tr : paths t

-- Starts a new trail and adds current trail to path
makeNewTrail :: (Ord n, Floating n) => TurtleState n
             -> TurtleState n
makeNewTrail t = t { currTrail = mempty `at` penPos t
                   , paths = addTrailToPath t (mapLoc wrapLine (currTrail t))
                   }

-- Modifies the current style after starting a new trail
modifyCurrStyle :: (Floating n, Ord n) =>
                   (PenStyle n -> PenStyle n)
                -> TurtleState n
                -> TurtleState n
modifyCurrStyle f t =  t # makeNewTrail # \t' -> t' { currPenStyle = (f . currPenStyle) t' }

-- Creates any TrailLike from a TurtlePath.
turtlePathToTrailLike :: (V t ~ V2, N t ~ n, FromTrail t) => TurtlePath n -> t
turtlePathToTrailLike (TurtlePath _ t) = fromLocTrail t

-- Creates a diagram from a TurtlePath using the provided styles
turtlePathToStroke :: TurtlePath Double -> Diagram V2
turtlePathToStroke t@(TurtlePath (PenStyle lineW lineC) _) = d
 where d = lc lineC .
           lw lineW
           $ turtlePathToTrailLike t
