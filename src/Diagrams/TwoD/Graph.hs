-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Graph
-- Copyright   :  (c) 2017 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- A library for drawing graphs. XXX
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Graph where

import           Diagrams.Prelude

{- API sketch:

   It is specific to 2D so we can do things like e.g. automatic
   best-effort positioning of vertex labels so as to avoid edges

   - Graph data type which has
     - set of vertices
     - set of edges (ordered pairs of vertices)
     - map vertex -> label   (a LABEL can be either text or a diagram; optionally + offset)
     - map edge   -> label
     - default vertex render   (function that takes contextual info + produces diagram)
     - map vertex -> render
     - default edge render    (ditto)
     - map edge -> render
     - map vertex -> point

   Can create an initial graph any way you like, e.g. using alga
   library and exporting graph data

mkDGraph, mkUGraph :: Set x -> Set (x,x) -> Graph b x

addDEdge :: x -> x -> Graph b x -> Graph b x
addUEdge :: x -> x -> Graph b x -> Graph b x

  -- Maybe labels should be split out into a separate lib
data RawLabel b
  = TextLabel String
  | DiaLabel  (Diagram b)

data Label b = Label
  { rawLabel    :: RawLabel b
  , labelOffset :: Maybe (V2 Double)
  }

... functions for constructing labels ...

labelVertex :: x -> Label b -> Graph b x -> Graph b x
labelVertices :: (x -> Label b) -> Graph b x -> Graph b x
labelEdge ...
labelEdges ...

-}
