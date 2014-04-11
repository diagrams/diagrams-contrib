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

    -- * Debugging
  , traceTurtle

    -- * Diagram related
  , getTurtleDiagram
  , getTurtlePath
  ) where

import           Debug.Trace      (traceShow)

import           Diagrams.Prelude

-- | Style attributes associated with the turtle pen
data PenStyle = PenStyle
  { penWidth :: Double         -- ^ Width of pen. Default is 1.0
  , penColor :: Colour Double  -- ^ Color of pen. Default is @black@
  } deriving Show

-- | Turtle path type that captures a list of paths and the style attributes
-- associated with them
data TurtlePath = TurtlePath
  { penStyle    :: PenStyle            -- ^ Style
  , turtleTrail :: Located (Trail R2)  -- ^ Path
  } deriving Show

-- | Core turtle data type. A turtle needs to keep track of its current
-- position, like its position, heading etc., and all the paths that it has
-- traversed so far.
--
-- We need to record a new path, everytime an attribute like style, pen position
-- etc changes, so that we can separately track styles for each portion of the
-- eventual path that the turtle took.
data TurtleState = TurtleState
  { -- | State of the pen. @False@ means that turtle movements will not draw
    -- anything
    isPenDown    :: Bool
     -- | Current position. This is updated everytime the turtle moves
  , penPos       :: P2
     -- | Orientation of the turtle in 2D space, given in degrees
  , heading      :: Angle
     -- | Path traversed by the turtle so far, without any style or pen
     -- attributes changing
  , currTrail    :: Located (Trail' Line R2)
     -- | Current style of the pen
  , currPenStyle :: PenStyle
     -- | List of paths along with style information, traversed by the turtle
     -- previously
  , paths        :: [TurtlePath]
  } deriving Show

-- | Default pen style, with @penWidth@ set to 1.0 and @penColor@ set to black
defaultPenStyle :: PenStyle
defaultPenStyle = PenStyle 1.0 black

-- | The initial state of turtle. The turtle is located at the origin, at an
-- orientation of 0 degrees with its pen position down. The pen style is
-- @defaultPenStyle@.
startTurtle :: TurtleState
startTurtle = TurtleState True origin zeroV (mempty `at` origin) defaultPenStyle []

-- | Draw a segment along the turtle’s path and update its position. If the pen
-- is up, only the position is updated.
moveTurtle :: Segment Closed R2 -- ^ Segment representing the path to travel
           -> TurtleState       -- ^ Turtle to move
           -> TurtleState       -- ^ Resulting turtle
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
   newPenPos   =  pos .+^ segOffset rotatedSeg

-- | Move the turtle forward by @x@ units
forward :: Double       -- ^ Distance to move
        -> TurtleState  -- ^ Turtle to move
        -> TurtleState  -- ^ Resulting turtle
forward x = moveTurtle (straight $ r2 (x,0))

-- | Move the turtle backward by @x@ units
backward :: Double       -- ^ Distance to move
         -> TurtleState  -- ^ Turtle to move
         -> TurtleState  -- ^ Resulting turtle
backward x = moveTurtle (straight $ r2 (negate x, 0))

-- | Turn the turtle by applying the given function to its current orientation
-- (in degrees)
turnTurtle :: (Angle -> Angle)   -- ^ Transformation to apply on current orientation
           -> TurtleState    -- ^ Turtle to turn
           -> TurtleState    -- ^ Resulting turtle
turnTurtle f t@(TurtleState _ _ h _ _ _) = t { heading = f h  }

-- | Turn the turtle anti-clockwise (left)
left :: Double       -- ^ Degree of turn
     -> TurtleState  -- ^ Turtle to turn
     -> TurtleState  -- ^ Resulting turtle
left d = turnTurtle (^+^ (d @@ deg))

-- | Turn the turtle clockwise (right)
right :: Double       -- ^ Degree of turn
      -> TurtleState  -- ^ Turtle to turn
      -> TurtleState  -- ^ Resulting turtle
right d = turnTurtle (^-^ (d @@ deg))

-- | Turn the turtle to the given orientation, in degrees
setHeading :: Double       -- ^ Degree of orientation
           -> TurtleState  -- ^ Turtle to orient
           -> TurtleState  -- ^ Resulting turtle
setHeading d = turnTurtle (const $ d @@ deg)

-- | Sets the turtle orientation towards a given location.
towards :: P2           -- ^ Point to orient turtle towards
        -> TurtleState  -- ^ Turtle to orient
        -> TurtleState  -- ^ Resulting turtle
towards p  = setHeading =<< (360 *) . (/ tau) . uncurry atan2 . unr2 . (p .-.) . penPos

-- | Puts the turtle pen in “Up” mode. Turtle movements will not draw anything
--
-- Does nothing if the pen was already up. Otherwise, it creates a turtle with
-- the current trail added to @paths@.
penUp :: TurtleState  -- ^ Turtle to modify
      -> TurtleState  -- ^ Resulting turtle
penUp t
 | isPenDown t = t # makeNewTrail #  \t' -> t' { isPenDown = False }
 | otherwise   = t

-- | Puts the turtle pen in “Down” mode. Turtle movements will cause drawing to
-- happen
--
-- Does nothing if the pen was already down. Otherwise, starts a new trail
-- starting at the current position.
penDown :: TurtleState  -- ^ Turtle to modify
        -> TurtleState  -- ^ Resulting turtle
penDown t
  | isPenDown t = t
  | otherwise   = t # makeNewTrail #  \t' -> t' { isPenDown = True }

-- Start a new trail at current position
penHop :: TurtleState
       -> TurtleState
penHop t = t # makeNewTrail

-- Closes the current path, to the starting position of the current
-- trail. Has no effect when the pen is up.
closeCurrent :: TurtleState
             -> TurtleState
closeCurrent t
  | isPenDown t = t # closeTTrail
  | otherwise   = t
 where startPos = loc . currTrail $ t
       closeTTrail t'  = t' { penPos    = startPos
                            , currTrail = mempty `at` startPos
                            , paths     = addTrailToPath t'
                                            (mapLoc (wrapTrail . closeLine) $ currTrail t)
                            }

-- | Set the turtle X/Y position.
--
-- If pen is down and the current trail is non-empty, this will also add the
-- current trail to the @paths@ field.
setPenPos :: P2           -- ^ Position to place true
          -> TurtleState  -- ^ Turtle to position
          -> TurtleState  -- ^ Resulting turtle
setPenPos newPos t = t {penPos = newPos } # makeNewTrail

-- | Set a new pen width for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenWidth :: Double       -- ^ Width of Pen
            -> TurtleState  -- ^ Turtle to change
            -> TurtleState  -- ^ Resulting Turtle
setPenWidth w = modifyCurrStyle (\s -> s { penWidth = w })
-- | Set a new pen color for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenColour :: Colour Double  -- ^ Width of Pen
             -> TurtleState    -- ^ Turtle to change
             -> TurtleState    -- ^ Resulting Turtle
setPenColour c = modifyCurrStyle (\s -> s { penColor = c })

-- | alias of @setPenColour@
setPenColor :: Colour Double  -- ^ Width of Pen
            -> TurtleState    -- ^ Turtle to change
            -> TurtleState    -- ^ Resulting Turtle
setPenColor = setPenColour

-- | Creates a diagram from a turtle
--
-- Applies the styles to each trails in @paths@ separately and combines them
-- into a single diagram
getTurtleDiagram :: (Renderable (Path R2) b)
                 => TurtleState
                 -> Diagram b R2
getTurtleDiagram t =
  mconcat .
  map turtlePathToStroke .
  paths $ t # penUp -- Do a penUp to add @currTrail@ to @paths@

-- | Creates a path from a turtle, ignoring the styles.
getTurtlePath :: TurtleState -> Path R2
getTurtlePath = mconcat . map turtlePathToTrailLike . paths . penUp

-- * Helper functions

-- Makes a "TurtlePath" from a "Turtle"’s @currTrail@ field
makeTurtlePath :: TurtleState
               -> Located (Trail R2)
               -> TurtlePath
makeTurtlePath t tr = TurtlePath (currPenStyle t) tr

-- Returns a list of paths, with current trail added to a "Turtle"’s @paths@ field
addTrailToPath :: TurtleState
               -> Located (Trail R2)
               -> [TurtlePath]
addTrailToPath t tr
  | isTrailEmpty (unLoc tr) = paths t
  | otherwise               = makeTurtlePath t tr : paths t

-- Starts a new trail and adds current trail to path
makeNewTrail :: TurtleState
             -> TurtleState
makeNewTrail t = t { currTrail = mempty `at` penPos t
                   , paths = addTrailToPath t (mapLoc wrapTrail (currTrail t))
                   }

-- Modifies the current style after starting a new trail
modifyCurrStyle :: (PenStyle -> PenStyle)
                -> TurtleState
                -> TurtleState
modifyCurrStyle f t =  t # makeNewTrail # \t' -> t' { currPenStyle = (f . currPenStyle) t' }

-- Creates any TrailLike from a TurtlePath.
turtlePathToTrailLike :: (V t ~ R2, TrailLike t) => TurtlePath -> t
turtlePathToTrailLike (TurtlePath _ t) = trailLike t

-- Creates a diagram from a TurtlePath using the provided styles
turtlePathToStroke :: (Renderable (Path R2) b) => TurtlePath
                   -> Diagram b R2
turtlePathToStroke t@(TurtlePath (PenStyle lineWidth_  lineColor_) _) = d
 where d = lc lineColor_ .
           lwG lineWidth_ .
           stroke $ turtlePathToTrailLike t

-- | Prints out turtle representation and returns it. Use for debugging
traceTurtle :: TurtleState
            -> TurtleState
traceTurtle t = traceShow t t
