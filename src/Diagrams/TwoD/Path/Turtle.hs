{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ( Turtle, TurtleT

    -- * Turtle control commands
  , runTurtle, runTurtleT
  , drawTurtle, drawTurtleT
  , sketchTurtle, sketchTurtleT

    -- * Motion commands
  , forward, backward, left, right

    -- * State accessors / setters
  , heading, setHeading, towards, isDown
  , pos, setPos, setPenWidth, setPenColor

    -- * Drawing control
  , penUp, penDown, penHop, closeCurrent
  ) where

import qualified Control.Lens                       as L
import           Control.Monad                      (liftM)
import qualified Control.Monad.State                as ST

import           Diagrams.Prelude
import qualified Diagrams.TwoD.Path.Turtle.Internal as T


type TurtleT n = ST.StateT (T.TurtleState n)

type Turtle n = TurtleT n Identity

-- | A more general way to run the turtle. Returns a computation in the
-- underlying monad @m@ yielding the final turtle state.
runTurtleT :: (OrderedField n, Monad m) => TurtleT n m a -> m (T.TurtleState n)
runTurtleT t = ST.execStateT t T.startTurtle

-- | Run the turtle, yielding the final turtle state.
runTurtle :: (Floating n, Ord n) => Turtle n a -> T.TurtleState n
runTurtle = runIdentity . runTurtleT

-- | A more general way to run the turtle.  Returns a computation in
--   the underlying monad @m@ yielding the final diagram.
drawTurtleT :: (Monad m) => TurtleT Double m a -> m (Diagram V2)
drawTurtleT = liftM T.getTurtleDiagram . runTurtleT

-- | Run the turtle, yielding a diagram.
drawTurtle :: Turtle Double a -> Diagram V2
drawTurtle = runIdentity . drawTurtleT

-- | A more general way to run the turtle. Returns a computation in
--   the underlying monad @m@, ignoring any pen style commands and
--   yielding a 2D path.
sketchTurtleT :: (Monad m, Floating n, Ord n) => TurtleT n m a -> m (Path V2 n)
sketchTurtleT = liftM T.getTurtlePath . runTurtleT

-- | Run the turtle, ignoring any pen style commands and yielding a
--   2D path.
sketchTurtle :: (Floating n, Ord n) => Turtle n a -> Path V2 n
sketchTurtle = runIdentity . sketchTurtleT

-- Motion commands

-- | Move the turtle forward, along the current heading.
forward :: (OrderedField n, Monad m) => n -> TurtleT n m ()
forward x = ST.modify $ T.forward x

-- | Move the turtle backward, directly away from the current heading.
backward :: (OrderedField n, Monad m) => n -> TurtleT n m ()
backward x = ST.modify $ T.backward x

-- | Modify the current heading to the left by the specified angle in degrees.
left :: (OrderedField n, Monad m) => n -> TurtleT n m ()
left d = ST.modify $ T.left d

-- | Modify the current heading to the right by the specified angle in degrees.
right :: (OrderedField n, Monad m) => n -> TurtleT n m ()
right d = ST.modify $ T.right d

-- State accessors / setters

-- | Set the current turtle angle, in degrees.
setHeading :: (OrderedField n, Monad m) => n -> TurtleT n m ()
setHeading d = ST.modify $ T.setHeading d

-- | Get the current turtle angle, in degrees.
heading :: (OrderedField n, Monad m) => TurtleT n m n
heading = ST.gets (L.view deg . T.heading)

-- | Sets the heading towards a given location.
towards :: (Monad m, RealFloat n) => P2 n -> TurtleT n m ()
towards pt = ST.modify $ T.towards pt

-- | Set the current turtle X/Y position.
setPos :: (OrderedField n, Monad m) => P2 n -> TurtleT n m ()
setPos p = ST.modify $ T.setPenPos p

-- | Get the current turtle X/Y position.
pos :: Monad m => TurtleT n m (P2 n)
pos = ST.gets T.penPos

-- Drawing control.

-- | Ends the current path, and enters into "penUp" mode
penUp :: (OrderedField n, Monad m) => TurtleT n m ()
penUp   = ST.modify T.penUp

-- | Ends the current path, and enters into "penDown" mode
penDown :: (OrderedField n, Monad m) => TurtleT n m ()
penDown = ST.modify T.penDown

-- | Start a new trail at current position
penHop :: (OrderedField n, Monad m) => TurtleT n m ()
penHop = ST.modify T.penHop

-- | Queries whether the pen is currently drawing a path or not.
isDown :: Monad m => TurtleT n m Bool
isDown = ST.gets T.isPenDown

-- | Closes the current path , to the starting position of the current
-- trail. Has no effect when the pen position is up.
closeCurrent :: (OrderedField n, Monad m) => TurtleT n m ()
closeCurrent = ST.modify T.closeCurrent

-- | Sets the pen color
setPenColor :: (OrderedField n, Monad m) => Colour Double -> TurtleT n m ()
setPenColor c = ST.modify $ T.setPenColor c

-- | Sets the pen size
setPenWidth  :: (OrderedField n, Monad m) => Measure n -> TurtleT n m ()
setPenWidth s = ST.modify $ T.setPenWidth s
