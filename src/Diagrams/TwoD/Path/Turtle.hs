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
import           Control.Monad.Identity             (Identity (..))
import qualified Control.Monad.State                as ST

import           Diagrams.Prelude
import qualified Diagrams.TwoD.Path.Turtle.Internal as T


type TurtleT = ST.StateT T.TurtleState

type Turtle = TurtleT Identity

-- | A more general way to run the turtle. Returns a computation in the
-- underlying monad @m@ yielding the final turtle state.
runTurtleT :: Monad m => TurtleT m a -> m T.TurtleState
runTurtleT t = ST.execStateT t T.startTurtle

-- | Run the turtle, yielding the final turtle state.
runTurtle :: Turtle a -> T.TurtleState
runTurtle = runIdentity . runTurtleT

-- | A more general way to run the turtle.  Returns a computation in
--   the underlying monad @m@ yielding the final diagram.
drawTurtleT :: (Monad m, Functor m, Renderable (Path R2) b)
            => TurtleT m a -> m (Diagram b R2)
drawTurtleT = fmap T.getTurtleDiagram . runTurtleT

-- | Run the turtle, yielding a diagram.
drawTurtle :: (Renderable (Path R2) b) => Turtle a -> Diagram b R2
drawTurtle = runIdentity . drawTurtleT

-- | A more general way to run the turtle. Returns a computation in
--   the underlying monad @m@, ignoring any pen style commands and
--   yielding a 2D path.
sketchTurtleT :: (Functor m, Monad m) => TurtleT m a -> m (Path R2)
sketchTurtleT = fmap T.getTurtlePath . runTurtleT

-- | Run the turtle, ignoring any pen style commands and yielding a
--   2D path.
sketchTurtle :: Turtle a -> Path R2
sketchTurtle = runIdentity . sketchTurtleT

-- Motion commands

-- | Move the turtle forward, along the current heading.
forward :: Monad m => Double -> TurtleT m ()
forward x = ST.modify $ T.forward x

-- | Move the turtle backward, directly away from the current heading.
backward :: Monad m => Double -> TurtleT m ()
backward x = ST.modify $ T.backward x

-- | Modify the current heading to the left by the specified angle in degrees.
left :: Monad m => Double -> TurtleT m ()
left d = ST.modify $ T.left d

-- | Modify the current heading to the right by the specified angle in degrees.
right :: Monad m => Double -> TurtleT m ()
right d = ST.modify $ T.right d

-- State accessors / setters

-- | Set the current turtle angle, in degrees.
setHeading :: Monad m => Double -> TurtleT m ()
setHeading d = ST.modify $ T.setHeading d

-- | Get the current turtle angle, in degrees.
heading :: Monad m => TurtleT m Double
heading = ST.gets (L.view deg . T.heading)

-- | Sets the heading towards a given location.
towards :: Monad m => P2 -> TurtleT m ()
towards pt = ST.modify $ T.towards pt

-- | Set the current turtle X/Y position.
setPos :: Monad m => P2 -> TurtleT m ()
setPos p = ST.modify $ T.setPenPos p

-- | Get the current turtle X/Y position.
pos ::  Monad m => TurtleT m P2
pos = ST.gets T.penPos

-- Drawing control.

-- | Ends the current path, and enters into "penUp" mode
penUp :: Monad m => TurtleT m ()
penUp   = ST.modify T.penUp

-- | Ends the current path, and enters into "penDown" mode
penDown :: Monad m => TurtleT m ()
penDown = ST.modify T.penDown

-- | Start a new trail at current position
penHop :: Monad m => TurtleT m ()
penHop = ST.modify T.penHop

-- | Queries whether the pen is currently drawing a path or not.
isDown :: Monad m => TurtleT m Bool
isDown = ST.gets T.isPenDown

-- | Closes the current path , to the starting position of the current
-- trail. Has no effect when the pen position is up.
closeCurrent :: Monad m => TurtleT m ()
closeCurrent = ST.modify T.closeCurrent

-- | Sets the pen color
setPenColor :: Monad m => Colour Double -> TurtleT m ()
setPenColor c = ST.modify $ T.setPenColor c

-- | Sets the pen size
setPenWidth  :: Monad m =>  Double -> TurtleT m ()
setPenWidth s = ST.modify $ T.setPenWidth s
