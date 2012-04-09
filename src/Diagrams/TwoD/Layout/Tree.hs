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
-- Laying out a rose tree using a symmetric layout:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import Diagrams.Prelude
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJ")]]
-- >
-- > example =
-- >   renderTree ((<> circle 1 # fc white) . text . show)
-- >              (~~)
-- >              (symmLayout' with { slHSep = 4, slVSep = 4 } t1)
--
-- Laying out a rose tree of diagrams, with spacing automatically
-- adjusted for the size of the diagrams:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import Diagrams.Prelude
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > tD = Node (rect 1 3)
-- >        [ Node (circle 0.2) []
-- >        , Node (hcat . replicate 3 $ circle 1) []
-- >        , Node (eqTriangle 5) []
-- >        ]
-- >
-- > example =
-- >   renderTree id (~~)
-- >   (symmLayout' with { slWidth  = fromMaybe (0,0) . extentX
-- >                     , slHeight = fromMaybe (0,0) . extentY }
-- >      tD)
--
-- Using force-based layout on a binary tree:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import Diagrams.Prelude
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > t = BNode 1 (BNode 8 (leaf 7) (leaf 2)) (BNode 6 (leaf 3) (leaf 4))
-- >
-- > Just t' = uniqueXLayout 1 1 t
-- >
-- > example = renderTree (\n -> (text (show n) # fontSize 0.5
-- >                             <> circle 0.3 # fc white))
-- >             (~~)
-- >             (forceLayoutTree t')

module Diagrams.TwoD.Layout.Tree
       ( -- * Binary trees
         -- $BTree

         BTree(..)
       , leaf

         -- * Layout algorithms

         -- ** Binary tree layout

       , uniqueXLayout

         -- ** Symmetric rose tree layout

         -- $symmetric
       , symmLayout
       , symmLayout'
       , SymmLayoutOpts(..)

         -- ** Force-directed layout
         -- $forcedirected

       , forceLayoutTree
       , forceLayoutTree'
       , ForceLayoutTreeOpts(..)

       , treeToEnsemble
       , label
       , reconstruct

         -- * Rendering

       , renderTree

       ) where

import           Physics.ForceLayout

import           Control.Applicative
import           Control.Arrow         (first, second, (***), (&&&))
import           Control.Monad.State

import           Data.Default
import qualified Data.Foldable         as F
import qualified Data.Map              as M
import           Data.Label            (mkLabels)
import qualified Data.Label            as L
import           Data.List             (mapAccumL)
import           Data.Maybe
import qualified Data.Traversable      as T
import           Data.Tree

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

--------------------------------------------------
-- Unique X layout for binary trees.  No
-- two nodes share the same X coordinate.

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
-- "Symmetric" layout of rose trees.

-- $symmetric
-- "Symmetric" layout of rose trees, based on the algorithm described in:
--
-- Andrew J. Kennedy. /Drawing Trees/, J Func. Prog. 6 (3): 527-534,
-- May 1996.
--
-- Trees laid out using this algorithm satisfy:
--
--   1. Nodes at a given level are always separated by at least a
--   given minimum distance.
--
--   2. Parent nodes are centered with respect to their immediate
--   offspring (though /not/ necessarily with respect to the entire
--   subtrees under them)
--
--   3. Layout commutes with mirroring: that is, the layout of a given
--   tree is the mirror image of the layout of the tree's mirror
--   image.
--
--   4. Identical subtrees are always rendered identically.  Put
--   another way, the layout of any subtree is independent of the rest
--   of the tree.
--
--   5. The layouts are as narrow as possible while satisfying all the
--   above constraints.

-- | A tree with /relative/ positioning information.  The Double
--   at each node is the horizontal /offset/ from its parent.
type RelTree a = Tree (a, Double)

-- | Shift a RelTree horizontally.
moveTree :: Double -> RelTree a -> RelTree a
moveTree x' (Node (a, x) ts) = Node (a, x+x') ts

-- | An /extent/ is a list of pairs, recording the leftmost and
--   rightmost (absolute) horizontal positions of a tree at each
--   depth.
type Extent = [(Double, Double)]

-- | Shift an extent horizontally.
moveExtent :: Double -> Extent -> Extent
moveExtent x = map ((+x) *** (+x))

-- | Reflect an extent about the vertical axis.
flipExtent :: Extent -> Extent
flipExtent = map (\(p,q) -> (-q, -p))

-- | Merge two non-overlapping extents.
mergeExtents :: Extent -> Extent -> Extent
mergeExtents [] qs = qs
mergeExtents ps [] = ps
mergeExtents ((p,_) : ps) ((_,q) : qs) = (p,q) : mergeExtents ps qs

mergeExtentList :: [Extent] -> Extent
mergeExtentList = foldr mergeExtents []

-- | Determine the amount to shift in order to \"fit\" two extents
--   next to one another.  The first argument is the separation to
--   leave between them.
fit :: Double -> Extent -> Extent -> Double
fit hSep ps qs = maximum (0 : zipWith (\(_,p) (q,_) -> p - q + hSep) ps qs)

-- | Fit a list of subtree extents together using a left-biased
--   algorithm.  Compute a list of positions (relative to the leftmost
--   subtree which is considered to have position 0).
fitListL :: Double -> [Extent] -> [Double]
fitListL hSep = snd . mapAccumL fitOne []
  where
    fitOne acc e =
      let x = fit hSep acc e
      in  (mergeExtents acc (moveExtent x e), x)

-- | Fit a list of subtree extents together with a right bias.
fitListR :: Double -> [Extent] -> [Double]
fitListR hSep = reverse . map negate . fitListL hSep . map flipExtent . reverse

-- | Compute a symmetric fitting by averaging the results of left- and
--   right-biased fitting.
fitList :: Double -> [Extent] -> [Double]
fitList hSep = uncurry (zipWith mean) . (fitListL hSep &&& fitListR hSep)
  where mean x y = (x+y)/2

-- | Actual recursive tree layout algorithm, which returns a tree
--   layout as well as an extent.
symmLayoutR :: SymmLayoutOpts a -> Tree a -> (RelTree a, Extent)
symmLayoutR opts (Node a ts) = (rt, ext)
  where (trees, extents) = unzip (map (symmLayoutR opts) ts)
        positions        = fitList (slHSep opts) extents
        pTrees           = zipWith moveTree positions trees
        pExtents         = zipWith moveExtent positions extents
        ext              = slWidth opts a : mergeExtentList pExtents
        rt               = Node (a, 0) pTrees

-- | Options for controlling the
data SymmLayoutOpts a =
  SLOpts { slHSep   :: Double           -- ^ Minimum horizontal
                                        --   separation between sibling
                                        --   nodes.  The default is 1.
         , slVSep   :: Double           -- ^ Vertical separation
                                        --   between adjacent levels of
                                        --   the tree.  The default is 1.
         , slWidth  :: a -> (Double, Double)
           -- ^ A function for measuring the horizontal extent (a pair
           --   of x-coordinates) of an item in the tree.  The default
           --   is @const (0,0)@, that is, the nodes are considered as
           --   taking up no space, so the centers of the nodes will
           --   be separated according to the @slHSep@ and @slVSep@.
           --   However, this can be useful, /e.g./ if you have a tree
           --   of diagrams of irregular size and want to make sure no
           --   diagrams overlap.  In that case you could use
           --   @fromMaybe (0,0) . extentX@.
         , slHeight :: a -> (Double, Double)
           -- ^ A function for measuring the vertical extent of an
           --   item in the tree.  The default is @const (0,0)@.  See
           --   the documentation for 'slWidth' for more information.
         }

instance Default (SymmLayoutOpts a) where
  def = SLOpts
          { slHSep   = 1
          , slVSep   = 1
          , slWidth  = const (0,0)
          , slHeight = const (0,0)
          }

-- | Run the symmetric rose tree layout algorithm on a given tree,
--   resulting in the same tree annotated with node positions.
symmLayout' :: SymmLayoutOpts a -> Tree a -> Tree (a, P2)
symmLayout' opts = unRelativize opts origin . fst . symmLayoutR opts

unRelativize :: SymmLayoutOpts a -> P2 -> RelTree a -> Tree (a, P2)
unRelativize opts curPt (Node (a,hOffs) ts)
    = Node (a, rootPt) (map (unRelativize opts (rootPt .+^ (vOffs *^ unit_Y))) ts)
  where rootPt = curPt .+^ (hOffs *^ unitX)
        vOffs  = - fst (slHeight opts a)
               + (maximum . map (snd . slHeight opts . fst . rootLabel) $ ts)
               + slVSep opts

-- | Run the symmetric rose tree layout algorithm on a given tree
--   using default options, resulting in the same tree annotated with
--   node positions.
symmLayout :: Tree a -> Tree (a, P2)
symmLayout = symmLayout' def

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
        swap (x,y) = (y,x)

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

instance Default ForceLayoutTreeOpts where
  def = FLTOpts
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

-- | Force-directed layout of rose trees, with default parameters (for
--   more options, see 'forceLayoutTree'').  In particular,
--
--   * edges are modeled as springs
--
--   * nodes are modeled as point charges
--
--   * nodes are constrained to keep the same y-coordinate.
--
--   The input could be a tree already laid out by some other method,
--   such as 'uniqueXLayout'.
forceLayoutTree :: Tree (a, P2) -> Tree (a, P2)
forceLayoutTree = forceLayoutTree' def

-- | Force-directed layout of rose trees, with configurable parameters.
forceLayoutTree' :: ForceLayoutTreeOpts -> Tree (a, P2) -> Tree (a, P2)
forceLayoutTree' opts t = reconstruct (forceLayout (forceLayoutOpts opts) e) ti
  where (ti, e) = treeToEnsemble opts t

------------------------------------------------------------
--  Rendering
------------------------------------------------------------

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.
renderTree :: Monoid' m
           => (a -> QDiagram b R2 m) -> (P2 -> P2 -> QDiagram b R2 m)
           -> Tree (a, P2) -> QDiagram b R2 m
renderTree renderNode renderEdge = alignT . centerX . renderTree'
  where
    renderTree' (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (map renderTree' cs)
      <> mconcat (map (renderEdge p . snd . rootLabel) cs)