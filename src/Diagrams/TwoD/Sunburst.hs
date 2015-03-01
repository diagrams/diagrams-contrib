{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Sunburst
-- Copyright   :  (c) 2013-14 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Generation of Sunburst Partitions. A radial view of a Treemap.
--
-- The partitions are created without examining the contents of the tree nodes
-- which allows us to create a sunburst for any @Tree a@. As a consequence we cannot
-- base the size or color of the sections on the data in the tree, but only
-- on depth and number of children. Of course the code could easily be adapted
-- to handle more specific tree data.
--
-- See John Stasko, Richard Catrambone, \"An evaluation of space-filling
-- information visualizations for depicting hierarchical structures\", 2000.
-- <http://www.cc.gatech.edu/~john.stasko/papers/ijhcs00.pdf>.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Sunburst
  ( --  * Sunburst
    sunburst'
  , sunburst
  , SunburstOpts(..)
  , radius
  , sectionWidth
  , colors
  ) where

import           Control.Lens       (makeLenses)

import           Data.Default.Class
import qualified Data.Foldable as F
import           Data.Tree
import           Diagrams.Prelude   hiding (radius)

data SunburstOpts n = SunburstOpts
  { _radius       :: n -- ^ Relative size of the root circle, usually 1.
  , _sectionWidth :: n -- ^ Relative width of the sections.
  , _colors       :: [Colour Double]} -- ^ Color list one for each ring.

instance Fractional n => Default (SunburstOpts n) where
  def = SunburstOpts
    { _radius       = 1.0
    , _sectionWidth = 0.3
    , _colors       = [ lightcoral, lightseagreen, paleturquoise
                      , lightsteelblue, plum, violet, coral, honeydew]}

makeLenses ''SunburstOpts

-- Section data: Will be stored in nodes of a new rose tree and used to
-- make each section of the sunburst partition.
data SData n = SData
  n-- section radius
  n-- section width
  (Direction V2 n)  -- start direction
  (Angle n)           -- sweep angle
  Int             -- number of sections
  (Colour Double) -- color

-- Make n sections (annular wedges) starting in direction d and sweeping a
sections :: (Renderable (Path V2 n) b, TypeableFloat n) =>
            SData n -> QDiagram b V2 n Any
sections (SData r s d a n c) = mconcat $ iterateN n (rotate theta) w
  where
    theta = a ^/ fromIntegral n
    w = annularWedge (s + r) r d theta # lc white # lwG 0.008 # fc c

-- Convert an arbitrary @Tree a@ to a @Tree SData@ storing the sections info
-- in the nodes. If color list is shorter than depth of tree than the first
-- color of the list is repeated. If the color list is empty, lightgray is used.
toTree :: Floating n =>
          SunburstOpts n -> Tree a -> Direction V2 n -> Angle n  -> Tree (SData n)
toTree (SunburstOpts r s []) x q1 q2 =
  toTree (SunburstOpts r s (repeat lightgray)) x q1 q2
toTree (SunburstOpts r s (c:cs)) (Node _ ts) d a = Node (SData r s d a n c) ts'
  where
    n = length ts
    dt =  a ^/ fromIntegral n
    qs = [rotate (fromIntegral i *^ dt ) d  | i <- [0..n]]
    fs = toTree (SunburstOpts(r + s) s (cs ++ [c]))
    ts' = zipWith3 fs ts (take (n-1) qs) (repeat dt)

-- | Take any @Tree a@ and @SunburstOpts@ and make a sunburst partition.
--   Basically a treemap with a radial layout.
--   The root is the center of the sunburst and its circumference is divided
--   evenly according to the number of child nodes it has. Then each of those
--   sections is treated the same way.
sunburst' :: (Renderable (Path V2 n) b, TypeableFloat n) =>
             SunburstOpts n -> Tree a -> QDiagram b V2 n Any
sunburst' opts t = sunB $ toTree opts t xDir fullTurn
  where sunB (Node sd ts') = sections sd <> F.foldMap sunB ts'

-- | @sunburst@ with default opts
--
--   > import Diagrams.TwoD.Sunburst
--   > import Data.Tree (unfoldTree)
--   > aTree = unfoldTree (\n -> (0, replicate n (n-1))) 6
--   > sunburstEx = sunburst aTree # pad 1.1
--
--   <<diagrams/src_Diagrams_TwoD_Sunburst_sunburstEx.svg#diagram=sunburstEx&width=500>>
sunburst :: (Renderable (Path V2 n) b, TypeableFloat n) => Tree a -> QDiagram b V2 n Any
sunburst = sunburst' def
