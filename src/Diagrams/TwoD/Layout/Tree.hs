{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

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
-- > import Data.Tree
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJ")]]
-- >   where lf x = Node x []
-- >
-- > exampleSymmTree =
-- >   renderTree ((<> circle 1 # fc white) . text . (:[]))
-- >              (~~)
-- >              (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t1)
-- >   # centerXY # pad 1.1
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Tree_exampleSymmTree.svg#diagram=exampleSymmTree&width=300>>
--
-- Laying out a rose tree of diagrams, with spacing automatically
-- adjusted for the size of the diagrams:
--
-- > import Data.Tree
-- > import Data.Maybe (fromMaybe)
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > tD = Node (rect 1 3)
-- >        [ Node (circle 0.2) []
-- >        , Node (hcat . replicate 3 $ circle 1) []
-- >        , Node (eqTriangle 5) []
-- >        ]
-- >
-- > exampleSymmTreeWithDs =
-- >   renderTree id (~~)
-- >   (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
-- >                      & slHeight .~ fromMaybe (0,0) . extentY)
-- >      tD)
-- >   # centerXY # pad 1.1
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Tree_exampleSymmTreeWithDs.svg#diagram=exampleSymmTreeWithDs&width=300>>
--
-- Using a variant symmetric layout algorithm specifically for binary trees:
--
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > drawT = maybe mempty (renderTree (const (circle 0.05 # fc black)) (~~))
-- >       . symmLayoutBin' (with & slVSep .~ 0.5)
-- >
-- > tree500 = drawT t # centerXY # pad 1.1 # sized (Width 4)
-- >   where t = genTree 500 0.05
-- >         -- genTree 500 0.05 randomly generates trees of size 500 +/- 5%,
-- >         -- definition not shown
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Tree_tree500.svg#diagram=tree500&width=400>>
--
-- Using force-based layout on a binary tree:
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import Diagrams.Prelude
-- > import Diagrams.TwoD.Layout.Tree
-- >
-- > t 0 = Empty
-- > t n = BNode n (t (n-1)) (t (n-1))
-- >
-- > Just t' = uniqueXLayout 1 1 (t 4)
-- >
-- > fblEx = renderTree (\n -> (text (show n) # fontSizeL 0.5
-- >                             <> circle 0.3 # fc white))
-- >             (~~)
-- >             (forceLayoutTree t')
-- >         # centerXY # pad 1.1
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Tree_fblEx.svg#diagram=fblEx&width=300>>
--

module Diagrams.TwoD.Layout.Tree
       ( -- * Binary trees
         -- $BTree

         BTree(..)
       , leaf

         -- * Layout algorithms

         -- ** Unique-x layout

       , uniqueXLayout

         -- ** Symmetric layout

         -- $symmetric
       , symmLayout
       , symmLayout'
       , symmLayoutBin
       , symmLayoutBin'
       , SymmLayoutOpts(..), slHSep, slVSep, slWidth, slHeight

         -- ** Force-directed layout
         -- $forcedirected

       , forceLayoutTree
       , forceLayoutTree'
       , ForceLayoutTreeOpts(..), forceLayoutOpts, edgeLen, springK, staticK

       , treeToEnsemble
       , label
       , reconstruct

         -- * Rendering

       , renderTree
       , renderTree'

       ) where

import           Physics.ForceLayout

import           Control.Applicative
import           Control.Arrow       (first, second, (&&&), (***))
import           Control.Monad.State

import           Data.Default
import qualified Data.Foldable       as F
import           Data.Function       (on)
import           Data.List           (mapAccumL)
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Traversable    as T
import           Data.Tree

import           Diagrams.Prelude



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

makeLenses ''Pos

pos2Point :: Num n => n -> n -> Pos -> P2 n
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

uniqueXLayout :: Num n => n -> n -> BTree a -> Maybe (Tree (a, P2 n))
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
        mkNode = get <* (horiz += 1)

        down = level += 1
        up   = level -= 1

--------------------------------------------------
-- "Symmetric" layout of rose trees.

-- $symmetric
-- \"Symmetric\" layout of rose trees, based on the algorithm described in:
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
--   subtrees under them).
--
--   3. Layout commutes with mirroring: that is, the layout of a given
--   tree is the mirror image of the layout of the tree's mirror
--   image.  Put another way, there is no inherent left or right bias.
--
--   4. Identical subtrees are always rendered identically.  Put
--   another way, the layout of any subtree is independent of the rest
--   of the tree.
--
--   5. The layouts are as narrow as possible while satisfying all the
--   above constraints.

-- | A tree with /relative/ positioning information.  The @n@
--   at each node is the horizontal /offset/ from its parent.
type Rel t n a = t (a, n)

-- | Shift a RelTree horizontally.
moveTree :: Num n => n -> Rel Tree n a -> Rel Tree n a
moveTree x' (Node (a, x) ts) = Node (a, x+x') ts

-- | An /extent/ is a list of pairs, recording the leftmost and
--   rightmost (absolute) horizontal positions of a tree at each
--   depth.
newtype Extent n = Extent { getExtent :: [(n, n)] }

extent :: ([(n, n)] -> [(n, n)]) -> Extent n -> Extent n
extent f = Extent . f . getExtent

consExtent :: (n, n) -> Extent n -> Extent n
consExtent = extent . (:)

-- | Shift an extent horizontally.
moveExtent :: Num n => n -> Extent n -> Extent n
moveExtent x = (extent . map) ((+x) *** (+x))

-- | Reflect an extent about the vertical axis.
flipExtent :: Num n => Extent n -> Extent n
flipExtent = (extent . map) (\(p,q) -> (-q, -p))

-- | Merge two non-overlapping extents.
mergeExtents :: Extent n -> Extent n -> Extent n
mergeExtents (Extent e1) (Extent e2) = Extent $ mergeExtents' e1 e2
  where

    mergeExtents' [] qs = qs
    mergeExtents' ps [] = ps
    mergeExtents' ((p,_) : ps) ((_,q) : qs) = (p,q) : mergeExtents' ps qs

instance Semigroup (Extent n) where
  (<>) = mergeExtents

instance Monoid (Extent n) where
  mempty  = Extent []
  mappend = (<>)

-- | Determine the amount to shift in order to \"fit\" two extents
--   next to one another.  The first argument is the separation to
--   leave between them.
fit :: (Num n, Ord n) => n -> Extent n -> Extent n -> n
fit hSep (Extent ps) (Extent qs) = maximum (0 : zipWith (\(_,p) (q,_) -> p - q + hSep) ps qs)

-- | Fit a list of subtree extents together using a left-biased
--   algorithm.  Compute a list of positions (relative to the leftmost
--   subtree which is considered to have position 0).
fitListL :: (Num n, Ord n) => n -> [Extent n] -> [n]
fitListL hSep = snd . mapAccumL fitOne mempty
  where
    fitOne acc e =
      let x = fit hSep acc e
      in  (acc <> moveExtent x e, x)

-- | Fit a list of subtree extents together with a right bias.
fitListR :: (Num n, Ord n) => n -> [Extent n] -> [n]
fitListR hSep = reverse . map negate . fitListL hSep . map flipExtent . reverse

-- | Compute a symmetric fitting by averaging the results of left- and
--   right-biased fitting.
fitList :: (Fractional n, Ord n) => n -> [Extent n] -> [n]
fitList hSep = uncurry (zipWith mean) . (fitListL hSep &&& fitListR hSep)
  where mean x y = (x+y)/2

-- | Options for controlling the symmetric tree layout algorithm.
data SymmLayoutOpts n a =
  SLOpts { _slHSep   :: n -- ^ Minimum horizontal
                                         --   separation between sibling
                                         --   nodes.  The default is 1.
         , _slVSep   :: n -- ^ Vertical separation
                                         --   between adjacent levels of
                                         --   the tree.  The default is 1.
         , _slWidth  :: a -> (n, n)
           -- ^ A function for measuring the horizontal extent (a pair
           --   of x-coordinates) of an item in the tree.  The default
           --   is @const (0,0)@, that is, the nodes are considered as
           --   taking up no space, so the centers of the nodes will
           --   be separated according to the @slHSep@ and @slVSep@.
           --   However, this can be useful, /e.g./ if you have a tree
           --   of diagrams of irregular size and want to make sure no
           --   diagrams overlap.  In that case you could use
           --   @fromMaybe (0,0) . extentX@.
         , _slHeight :: a -> (n, n)
           -- ^ A function for measuring the vertical extent of an
           --   item in the tree.  The default is @const (0,0)@.  See
           --   the documentation for 'slWidth' for more information.
         }

makeLenses ''SymmLayoutOpts

instance Num n => Default (SymmLayoutOpts n a) where
  def = SLOpts
          { _slHSep   = 1
          , _slVSep   = 1
          , _slWidth  = const (0,0)
          , _slHeight = const (0,0)
          }

-- | Actual recursive tree layout algorithm, which returns a tree
--   layout as well as an extent.
symmLayoutR :: (Fractional n, Ord n) => SymmLayoutOpts n a -> Tree a -> (Rel Tree n a, Extent n)
symmLayoutR opts (Node a ts) = (rt, ext)
  where (trees, extents) = unzip (map (symmLayoutR opts) ts)
        positions        = fitList (opts ^. slHSep) extents
        pTrees           = zipWith moveTree positions trees
        pExtents         = zipWith moveExtent positions extents
        ext              = (opts^.slWidth) a `consExtent` mconcat pExtents
        rt               = Node (a, 0) pTrees

-- | Symmetric tree layout algorithm specialized to binary trees.
--   Returns a tree layout as well as an extent.
symmLayoutBinR :: (Fractional n, Ord n) =>
                  SymmLayoutOpts n a -> BTree a -> (Maybe (Rel Tree n a), Extent n)
symmLayoutBinR _    Empty         = (Nothing, mempty)
symmLayoutBinR opts (BNode a l r) = (Just rt, ext)
  where (l', extL) = symmLayoutBinR opts l
        (r', extR) = symmLayoutBinR opts r
        positions  = case (l', r') of
                       (Nothing, _) -> [0, opts ^. slHSep / 2]
                       (_, Nothing) -> [-(opts ^. slHSep) / 2, 0]
                       _          -> fitList (opts ^. slHSep) [extL, extR]
        pTrees   = catMaybes $ zipWith (fmap . moveTree) positions [l',r']
        pExtents = zipWith moveExtent positions [extL, extR]
        ext = (opts^.slWidth) a `consExtent` mconcat pExtents
        rt  = Node (a, 0) pTrees

-- | Run the symmetric rose tree layout algorithm on a given tree,
--   resulting in the same tree annotated with node positions.
symmLayout' :: (Fractional n, Ord n) => SymmLayoutOpts n a -> Tree a -> Tree (a, P2 n)
symmLayout' opts = unRelativize opts origin . fst . symmLayoutR opts

-- | Run the symmetric rose tree layout algorithm on a given tree
--   using default options, resulting in the same tree annotated with
--   node positions.
symmLayout :: (Fractional n, Ord n) => Tree a -> Tree (a, P2 n)
symmLayout = symmLayout' def

-- | Lay out a binary tree using a slight variant of the symmetric
--   layout algorithm.  In particular, if a node has only a left child
--   but no right child (or vice versa), the child will be offset from
--   the parent horizontally by half the horizontal separation
--   parameter. Note that the result will be @Nothing@ if and only if
--   the input tree is @Empty@.
symmLayoutBin' :: (Fractional n, Ord n) => SymmLayoutOpts n a -> BTree a -> Maybe (Tree (a,P2 n))
symmLayoutBin' opts = fmap (unRelativize opts origin) . fst . symmLayoutBinR opts

-- | Lay out a binary tree using a slight variant of the symmetric
--   layout algorithm, using default options.  In particular, if a
--   node has only a left child but no right child (or vice versa),
--   the child will be offset from the parent horizontally by half the
--   horizontal separation parameter. Note that the result will be
--   @Nothing@ if and only if the input tree is @Empty@.
symmLayoutBin :: (Fractional n, Ord n) => BTree a -> Maybe (Tree (a,P2 n))
symmLayoutBin = symmLayoutBin' def

-- | Given a fixed location for the root, turn a tree with
--   \"relative\" positioning into one with absolute locations
--   associated to all the nodes.
unRelativize :: (Num n, Ord n) =>
                SymmLayoutOpts n a -> P2 n -> Rel Tree n a -> Tree (a, P2 n)
unRelativize opts curPt (Node (a,hOffs) ts)
    = Node (a, rootPt) (map (unRelativize opts (rootPt .+^ (vOffs *^ unit_Y))) ts)
  where rootPt = curPt .+^ (hOffs *^ unitX)
        vOffs  = - fst ((opts^.slHeight) a)
               + (maximum . map (snd . (opts^.slHeight) . fst . rootLabel) $ ts)
               + (opts ^. slVSep)

--------------------------------------------------
--  Force-directed layout of rose trees

-- $forcedirected
-- Force-directed layout of rose trees.

data ForceLayoutTreeOpts n =
  FLTOpts
  { _forceLayoutOpts :: ForceLayoutOpts n -- ^ Options to the force layout simulator, including damping.
  , _edgeLen         :: n -- ^ How long edges should be, ideally.
                                           --   This will be the resting length for
                                           --   the springs.
  , _springK         :: n -- ^ Spring constant.  The
                                           --   bigger the constant,
                                           --   the more the edges
                                           --   push/pull towards their
                                           --   resting length.
  , _staticK         :: n -- ^ Coulomb constant.  The
                                           --   bigger the constant, the
                                           --   more sibling nodes repel
                                           --   each other.
  }

makeLenses ''ForceLayoutTreeOpts

instance Floating n => Default (ForceLayoutTreeOpts n) where
  def = FLTOpts
    { _forceLayoutOpts = def
    , _edgeLen = sqrt 2
    , _springK = 0.05
    , _staticK = 0.1
    }

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
--   The input to @treeToEnsemble@ could be a tree already laid out by
--   some other method, such as 'uniqueXLayout'.
treeToEnsemble :: forall a n. Floating n => ForceLayoutTreeOpts n
               -> Tree (a, P2 n) -> (Tree (a, PID), Ensemble V2 n)
treeToEnsemble opts t =
  ( fmap (first fst) lt
  , Ensemble
      [ (edges, \pt1 pt2 -> project unitX (hookeForce (opts ^. springK) (opts ^. edgeLen) pt1 pt2))
      , (sibs,  \pt1 pt2 -> project unitX (coulombForce (opts ^. staticK) pt1 pt2))
      ]
      particleMap
  )

  where lt :: Tree ((a,P2 n), PID)
        lt = label t

        particleMap :: M.Map PID (Particle V2 n)
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
reconstruct :: (Functor t, Num n) => Ensemble V2 n -> t (a, PID) -> t (a, P2 n)
reconstruct e = (fmap . second)
                  (fromMaybe origin . fmap (view pos) . flip M.lookup (e^.particles))

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
forceLayoutTree :: (Floating n, Ord n) => Tree (a, P2 n) -> Tree (a, P2 n)
forceLayoutTree = forceLayoutTree' def

-- | Force-directed layout of rose trees, with configurable parameters.
forceLayoutTree' :: (Floating n, Ord n) =>
                    ForceLayoutTreeOpts n -> Tree (a, P2 n) -> Tree (a, P2 n)
forceLayoutTree' opts t = reconstruct (forceLayout (opts^.forceLayoutOpts) e) ti
  where (ti, e) = treeToEnsemble opts t

------------------------------------------------------------
--  Rendering
------------------------------------------------------------

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.
renderTree :: (Monoid' m, Floating n, Ord n)
           => (a -> QDiagram b V2 n m) -> (P2 n -> P2 n -> QDiagram b V2 n m)
           -> Tree (a, P2 n) -> QDiagram b V2 n m
renderTree n e = renderTree' n (e `on` snd)

-- | Draw a tree annotated with node positions, given functions
--   specifying how to draw nodes and edges.  Unlike 'renderTree',
--   this version gives the edge-drawing function access to the actual
--   values stored at the nodes rather than just their positions.
renderTree' :: (Monoid' m, Floating n, Ord n)
           => (a -> QDiagram b V2 n m) -> ((a,P2 n) -> (a,P2 n) -> QDiagram b V2 n m)
           -> Tree (a, P2 n) -> QDiagram b V2 n m
renderTree' renderNode renderEdge = alignT . centerX . renderTreeR
  where
    renderTreeR (Node (a,p) cs) =
         renderNode a # moveTo p
      <> mconcat (map renderTreeR cs)
      <> mconcat (map (renderEdge (a,p) . rootLabel) cs)


-- > -- Critical size-limited Boltzmann generator for binary trees (used in example)
-- >
-- > import           Control.Applicative
-- > import           Control.Lens                   hiding (( # ))
-- > import           Control.Monad.Random
-- > import           Control.Monad.Reader
-- > import           Control.Monad.State
-- > import           Control.Monad.Trans.Maybe
-- >
-- > genTreeCrit :: ReaderT Int (StateT Int (MaybeT (Rand StdGen))) (BTree ())
-- > genTreeCrit = do
-- >   r <- getRandom
-- >   if r <= (1/2 :: Double)
-- >     then return Empty
-- >     else atom >> (BNode () <$> genTreeCrit <*> genTreeCrit)
-- >
-- > atom :: ReaderT Int (StateT Int (MaybeT (Rand StdGen))) ()
-- > atom = do
-- >   targetSize <- ask
-- >   curSize <- get
-- >   when (curSize >= targetSize) mzero
-- >   put (curSize + 1)
-- >
-- > genOneTree :: Int -> Int -> Double -> Maybe (BTree ())
-- > genOneTree seed size eps =
-- >   case mt of
-- >     Nothing -> Nothing
-- >     Just (t,sz) -> if sz >= minSz then Just t else Nothing
-- >
-- >   where
-- >     g          = mkStdGen seed
-- >     sizeWiggle = floor $ fromIntegral size * eps
-- >     maxSz = size + sizeWiggle
-- >     minSz = size - sizeWiggle
-- >     mt = (evalRand ?? g) . runMaybeT . (runStateT ?? 0) . (runReaderT ?? maxSz)
-- >        $ genTreeCrit
-- >
-- > genTree' :: Int -> Int -> Double -> BTree ()
-- > genTree' seed size eps =
-- >   case (genOneTree seed size eps) of
-- >     Nothing -> genTree' (seed+1) size eps
-- >     Just t  -> t
-- >
-- > genTree :: Int -> Double -> BTree ()
-- > genTree = genTree' 0
