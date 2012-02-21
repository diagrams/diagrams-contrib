{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , TemplateHaskell
           , NoMonomorphismRestriction
           , ScopedTypeVariables
           , FlexibleContexts
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.Tree
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A collection of methods for laying out various kinds of trees.
-- This module is still experimental, and more layout methods will
-- probably be added over time.
--
-- Here is an example of using force-based layout on a binary tree:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- >
-- > import Diagrams.Prelude
-- > import Diagrams.Backend.Cairo.CmdLine
-- >
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > t = BNode 1 (BNode 8 (leaf 7) (leaf 2)) (BNode 6 (leaf 3) (leaf 4))
-- >
-- > main = do
-- >   let Just t' = uniqueXLayout 1 1 t
-- >       t'' = forceLayoutTree defaultForceLayoutTreeOpts t'
-- >
-- >   defaultMain $
-- >     renderTree (\n -> (text (show n) # fontSize 0.5
-- >                        <> circle 0.3 # fc white))
-- >                (~~)
-- >                t''
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Layout.Tree
       ( -- * Binary trees
         -- $BTree

         BTree(..)
       , leaf

         -- * Layout algorithms

         -- ** Binary tree layout

       , uniqueXLayout

         -- ** Force-directed layout
         -- $forcedirected

       , forceLayoutTree
       , ForceLayoutTreeOpts(..)
       , defaultForceLayoutTreeOpts

       , treeToEnsemble
       , label
       , reconstruct

         -- * Rendering

       , renderTree

       ) where

import           Physics.ForceLayout

import           Control.Applicative
import           Control.Arrow         (first, second)
import           Control.Monad.State

import qualified Data.Foldable         as F
import qualified Data.Map              as M
import           Data.Label            (mkLabels)
import qualified Data.Label            as L
import           Data.Maybe
import qualified Data.Traversable      as T
import           Data.Tree
import           Data.Tuple

import           Diagrams.Prelude      hiding (e)

------------------------------------------------------------
--  Binary trees
------------------------------------------------------------

-- $BTree
-- There is a standard type of rose trees ('Tree') defined in the
-- @containers@ package, but there is no standard type for binary
-- trees, so we define one here.  Note, if you want to draw binary
-- trees with data of type @a@ at the leaves, you can use something
-- like @BTree (Maybe a)@ with @Nothing@ at internal nodes;
-- 'renderTree' lets you specify how to draw each node.

-- | Binary trees with data at internal nodes.
data BTree a = Empty | BNode a (BTree a) (BTree a)
  deriving (Eq, Ord, Read, Show, Functor, F.Foldable, T.Traversable)

-- | Convenient constructor for leaves.
leaf :: a -> BTree a
leaf a = BNode a Empty Empty

------------------------------------------------------------
--  Layout algorithms
------------------------------------------------------------

-- Unique X layout for binary trees.  No two nodes share the same X
-- coordinate.

data Pos = Pos { _level :: Int
               , _horiz :: Int
               }
  deriving (Eq, Show)

mkLabels [''Pos]

incHoriz, up, down :: MonadState Pos m => m ()
incHoriz   = modify (L.modify horiz (+1))
up         = modLevel (subtract 1)
down       = modLevel (+1)

modLevel :: MonadState Pos m => (Int -> Int) -> m ()
modLevel f = modify (L.modify level f)

pos2Point :: Double -> Double -> Pos -> P2
pos2Point cSep lSep (Pos l h) = p2 (fromIntegral h * cSep, -fromIntegral l * lSep)

--------------------------------------------------
-- Unique X layout for binary trees.  No
-- two nodes share the same X coordinate.

-- | @uniqueXLayout xSep ySep t@ lays out the binary tree @t@ using a
--   simple recursive algorithm with the following properties:
--
--   * Every left subtree is completely to the left of its parent, and
--     similarly for right subtrees.
--
--   * All the nodes at a given depth in the tree have the same
--     y-coordinate. The separation distance between levels is given by
--     @ySep@.
--
--   * Every node has a unique x-coordinate. The separation between
--     successive nodes from left to right is given by @xSep@.

uniqueXLayout :: Double -> Double -> BTree a -> Maybe (Tree (a, P2))
uniqueXLayout cSep lSep t = (fmap . fmap . second) (pos2Point cSep lSep)
                $ evalState (uniqueXLayout' t) (Pos 0 0)
  where uniqueXLayout' Empty         = return Nothing
        uniqueXLayout' (BNode a l r) = do
          down
          l' <- uniqueXLayout' l
          up
          p  <- mkNode
          down
          r' <- uniqueXLayout' r
          up
          return $ Just (Node (a,p) (catMaybes [l', r']))
        mkNode = get <* incHoriz

--------------------------------------------------
--  Force-directed layout of rose trees

-- $forcedirected
-- Force-directed layout of rose trees.

-- | Assign unique ID numbers to the nodes of a tree, and generate an
--   'Ensemble' suitable for simulating in order to do force-directed
--   layout of the tree.  In particular,
--
--   * edges are modeled as springs
--
--   * nodes are modeled as point charges
--
--   * nodes are constrained to keep the same y-coordinate.
--
--   The input to @treeToEnsemble@
treeToEnsemble :: forall a. ForceLayoutTreeOpts
               -> Tree (a, P2) -> (Tree (a, PID), Ensemble R2)
treeToEnsemble opts t =
  ( fmap (first fst) lt
  , Ensemble
      [ (edges, \pt1 pt2 -> project unitX (hookeForce (springK opts) (edgeLen opts) pt1 pt2))
      , (sibs,  \pt1 pt2 -> project unitX (coulombForce (staticK opts) pt1 pt2))
      ]
      particleMap
  )

  where lt :: Tree ((a,P2), PID)
        lt = label t

        particleMap :: M.Map PID (Particle R2)
        particleMap = M.fromList
                    . map (second initParticle)
                    . F.toList
                    . fmap (swap . first snd)
                    $ lt

        edges, sibs :: [Edge]
        edges       = extractEdges (fmap snd lt)
        sibs        = extractSibs [fmap snd lt]

        extractEdges :: Tree PID -> [Edge]
        extractEdges (Node i cs) = map (((,) i) . rootLabel) cs
                                    ++ concatMap extractEdges cs

        extractSibs :: Forest PID -> [Edge]
        extractSibs [] = []
        extractSibs ts = (\is -> zip is (tail is)) (map rootLabel ts)
                      ++ extractSibs (concatMap subForest ts)

--        sz = ala Sum foldMap . fmap (const 1) $ t
--        sibs = [(x,y) | x <- [0..sz-2], y <- [x+1 .. sz-1]]

-- | Assign unique IDs to every node in a tree (or other traversable structure).
label :: (T.Traversable t) => t a -> t (a, PID)
label = flip evalState 0 . T.mapM (\a -> get >>= \i -> modify (+1) >> return (a,i))

-- | Reconstruct a tree (or any traversable structure) from an
--   'Ensemble', given unique identifier annotations matching the
--   identifiers used in the 'Ensemble'.
reconstruct :: Functor t => Ensemble R2 -> t (a, PID) -> t (a, P2)
reconstruct e = (fmap . second)
                  (fromMaybe origin . fmap (L.get pos) . flip M.lookup (L.get particles e))

data ForceLayoutTreeOpts =
  FLTOpts
  { forceLayoutOpts :: ForceLayoutOpts R2 -- ^ Options to the force layout simulator, including damping
  , edgeLen         :: Double             -- ^ How long edges should be, ideally.
                                          --   This will be the resting length for
                                          --   the springs.
  , springK         :: Double             -- ^ Spring constant.  The
                                          --   bigger the constant,
                                          --   the more the edges
                                          --   push/pull towards their
                                          --   resting length.
  , staticK         :: Double             -- ^ Coulomb constant.  The
                                          --   bigger the constant, the
                                          --   more sibling nodes repel
                                          --   each other.
  }

defaultForceLayoutTreeOpts :: ForceLayoutTreeOpts
defaultForceLayoutTreeOpts =
  FLTOpts
  { forceLayoutOpts =
      FLOpts
      { damping     = 0.8
      , energyLimit = Just 0.001
      , stepLimit   = Just 1000
      }
  , edgeLen = sqrt 2
  , springK = 0.05
  , staticK = 0.1
  }

-- | Force-directed layout of rose trees.  In particular,
--
--   * edges are modeled as springs
--
--   * nodes are modeled as point charges
--
--   * nodes are constrained to keep the same y-coordinate.
--
--   The input could be a tree already laid out by some other method,
--   such as 'uniqueXLayout'.
forceLayoutTree :: ForceLayoutTreeOpts -> Tree (a, P2) -> Tree (a, P2)
forceLayoutTree opts t = reconstruct (forceLayout (forceLayoutOpts opts) e) ti
  where (ti, e) = treeToEnsemble opts t

------------------------------------------------------------
--  Rendering
------------------------------------------------------------

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.
renderTree :: (a -> Diagram b R2) -> (P2 -> P2 -> Diagram b R2)
           -> Tree (a, P2) -> Diagram b R2
renderTree renderNode renderEdge = alignT . centerX . renderTree'
  where
    renderTree' (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (map renderTree' cs)
      <> mconcat (map (renderEdge p . snd . rootLabel) cs)
