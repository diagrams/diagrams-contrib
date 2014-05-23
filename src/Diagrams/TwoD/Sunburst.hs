{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Sunburst
-- Copyright   :  (c) 2013 Jeffrey Rosenbluth
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
  , SunburstOpts(..), radius, sectionWidth, colors
  ) where

import           Control.Lens        (makeLenses, (^.))

import           Data.Tree
import           Data.Foldable       (foldMap)
import           Data.Default.Class
import           Diagrams.Prelude    hiding (radius)

data SunburstOpts
  = SunburstOpts
    { _radius       :: Double -- ^ Relative size of the root circle, usually 1.
    , _sectionWidth :: Double -- ^ Relative width of the sections.
    , _colors       :: [Colour Double] -- ^ Color list one for each ring.
    }

instance Default SunburstOpts where
  def = SunburstOpts
        { _radius       = 1.0
        , _sectionWidth = 0.3
        , _colors       = [ lightcoral, lightseagreen, paleturquoise
                         ,lightsteelblue, plum, violet, coral, honeydew]
        }

makeLenses ''SunburstOpts

-- Section data: Will be stored in nodes of a new rose tree and used to
-- make each section of the sunburst partition.
-- radius, ring width, start direction, sweep angle, number of sections, color.
data SData = SData Double Double (Direction R2) Angle Int (Colour Double)

-- Make n sections (annular wedges) starting in direction d and sweeping a
sections :: Renderable (Path R2) b
        => Double -> Double -> Direction R2 -> Angle -> Int -> (Colour Double)
        -> Diagram b R2
sections r s d a n c = mconcat $ iterateN n (rotate theta) w
  where
    theta = a ^/ (fromIntegral n)
    w = annularWedge (s + r) r d theta
      # lc white # lwG 0.008 # fc c

-- Convert an arbitrary @Tree a@ to a @Tree SData@ storing the sections info
-- in the nodes. If color list is shorter than depth of tree than the first
-- color of the list is repeated. If the color list is empty, lightgray is used.
toTree :: Double -> Double-> [(Colour Double)] -> Tree a -> Direction R2 -> Angle  -> Tree SData
toTree r s [] x q1 q2 = toTree r s (repeat lightgray) x q1 q2
toTree r s (c:cs) (Node _ ts) d a
  = Node (SData r s d a n c) ts'
      where
        n = length ts
        dt =  a ^/ (fromIntegral n)
        qs = [d .+^ ((fromIntegral i) *^ dt ) | i <- [0..n]]
        fs = toTree (r + s) s (cs ++ [c])
        ts' = zipWith3 fs ts (take (n-1) qs) (repeat dt)

-- | Take any @Tree a@ and @SunburstOpts@ and make a sunburst partition.
--   Basically a treemap with a radial layout.
--   The root is the center of the sunburst and its circumference is divided
--   evenly according to the number of child nodes it has. Then each of those
--   sections is treated the same way.
sunburst' :: Renderable (Path R2) b => SunburstOpts -> Tree a -> Diagram b R2
sunburst' opts t
  = sunB $ toTree r s cs t xDir fullTurn
      where
        r = opts^.radius
        s = opts^.sectionWidth
        cs = opts^.colors
        sunB (Node (SData r' m d a n c) ts')
          = sections r' m d a n c <> (foldMap sunB ts')

-- | @sunburst@ with default opts
--
--   > import Diagrams.TwoD.Sunburst
--   > import Data.Tree (unfoldTree)
--   > aTree = unfoldTree (\n -> (0, replicate n (n-1))) 6
--   > sunburstEx = sunburst aTree # pad 1.1
--
--   <<diagrams/src_Diagrams_TwoD_Sunburst_sunburstEx.svg#diagram=sunburstEx&width=500>>
sunburst :: Renderable (Path R2) b => Tree a -> Diagram b R2
sunburst = sunburst' def
