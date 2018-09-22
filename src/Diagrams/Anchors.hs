{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Anchors
-- Copyright   :  (c) 2016 Bradley Hardy
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  bradleyhardy@live.com
--
-- An /anchor/ is a point on an object which can be used for alignment
-- by naming it, offering easier control over alignment compared to the
-- 'Diagrams.Align' module when aligning many objects.
--
-----------------------------------------------------------------------------

module Diagrams.Anchors
       (
         -- * Anchors
         Anchor
         -- * Anchored objects
       , Anchored
       , withAnchors
       , noAnchors
       , addAnchor
       , deleteAnchor
       , getAnchorOffset
       , alignAnchor
       , hasAnchor
       , unanchor
         -- * Positional anchors
       , PositionalAnchor (..)
       , rotateAnchors
       , rotatePosAnchors
         -- * Easily concatenate many anchored objects
       , anchorMany
       , anchorMany_
         -- * Debugging
       , showAnchor
       , showAnchor_)
       where

import           Diagrams.Types.Names
import           Diagrams.Types
import           Geometry
import           Diagrams.TwoD.Model

import qualified Control.Lens     as Lens
import           Control.Lens     hiding (transform, (.>))
import           Data.List        (foldl')
-- import           Data.Map         (Map)
-- import qualified Data.Map         as Map
import           Data.Maybe       (fromJust, fromMaybe)
import qualified Data.HashSet     as HS
import           Data.Typeable    (Typeable)
import           Data.Semigroup

import GHC.Generics (Generic)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable

import           Linear.Vector

--------------------------------------------------------------------------------
--  Anchors
--------------------------------------------------------------------------------

type Anchor = Name

--------------------------------------------------------------------------------
--  Anchored objects
--------------------------------------------------------------------------------

-- | An 'Anchored' object which can be aligned to anchor points before
-- concatenating with other 'Anchored' objects. Note that when concatenating,
-- any anchors with the same names in each of the left and right operands will
-- be retained in the left operand, and lost in the right. To avoid this, qualify
-- anchors in each object using '(\>>/)'.
data Anchored t =
  Anchored
  { _currentAnchor :: Maybe Anchor
  , _anchors :: HashMap Anchor (V t (N t))
  , _anchoredObj :: t
  }

makeLenses ''Anchored

type instance N (Anchored t) = N t
type instance V (Anchored t) = V t

instance (HasOrigin t, Additive (V t), Num (N t)) => HasOrigin (Anchored t) where
  moveOriginTo p@(P v) =
    (anchoredObj %~ moveOriginTo p) .
    (anchors . traverse %~ (^-^ v))

instance (InSpace v n t, Foldable v, Transformable t) => Transformable (Anchored t) where
  transform t =
    (anchors . traverse %~ apply t) .
    (anchoredObj %~ transform t)

instance (Additive (V t), Num (N t), HasOrigin t, Semigroup t) => Semigroup (Anchored t) where
  o1 <> o2 =
    let updateObj obj
          | Just anchor <- obj^.currentAnchor
            = moveOriginBy (getAnchorOffset anchor obj)
            . deleteAnchor anchor
            $ obj
          | otherwise = obj

        a1 <+> a2 = Anchored Nothing
                             ((a1 ^. anchors) <> (a2 ^. anchors))
                             ((a1 ^. anchoredObj) <> (a2 ^. anchoredObj))
    in updateObj o1 <+> updateObj o2

instance (Additive (V t), Num (N t), Semigroup t, HasOrigin t, Monoid t) => Monoid (Anchored t) where
  mempty = Anchored Nothing mempty mempty
  mappend = (<>)

instance (Show (V t (N t)), Show t) => Show (Anchored t) where
  showsPrec p anch =
    showsPrec p (anch^.anchors) . (", " ++) . showsPrec p (anch^.anchoredObj)

-- | Add another anchor to an already 'Anchored' object.
addAnchor :: IsName anchor => anchor -> V t (N t) -> Anchored t -> Anchored t
addAnchor anchor val = anchors . Lens.at (toName anchor) .~ Just val

-- | Attach a list of anchors to an object, making it 'Anchored'.
withAnchors :: IsName anchor => [(anchor, V t (N t))] -> t -> Anchored t
withAnchors = Anchored Nothing . HM.fromList . over (each . _1) toName

-- | Turn an object into a trivial 'Anchored' object with no anchors.
noAnchors :: t -> Anchored t
noAnchors = Anchored Nothing mempty

-- | Delete an anchor from an anchored object. Does nothing if the object does
-- not have the specified anchor.
deleteAnchor :: IsName anchor => anchor -> Anchored t -> Anchored t
deleteAnchor anchor = anchors . Lens.at (toName anchor) .~ Nothing

-- | Get the offset from the origin of a particular anchor, or 'zero' if the object
-- does not have the specified anchor.
getAnchorOffset :: (Num (N t), Additive (V t), IsName a) => a -> Anchored t -> V t (N t)
getAnchorOffset anchor = view $ anchors . Lens.at (toName anchor) . to (fromMaybe zero)

-- | Align an anchored object to an anchor. Subsequently concatening with '(<>)'
-- will take this into account.
alignAnchor :: (IsName a) => a -> Anchored t -> Anchored t
alignAnchor anch = currentAnchor .~ Just (toName anch)

-- | Does the given anchored object have the given anchor?
hasAnchor :: (IsName a) => a -> Anchored t -> Bool
hasAnchor anchor = view $ anchors . to (HM.member (toName anchor))

-- | Throw away anchors and get the underlying object.
unanchor
  :: Anchored t -> t
unanchor = view anchoredObj

--------------------------------------------------------------------------------
--  Positional Anchors
--------------------------------------------------------------------------------

-- | A convenient type of positional anchors.
data PositionalAnchor
  = AnchorL
  | AnchorTL
  | AnchorT
  | AnchorTR
  | AnchorR
  | AnchorBR
  | AnchorB
  | AnchorBL
  deriving (Eq, Ord, Show, Typeable, Enum, Generic, Hashable)

instance IsName PositionalAnchor where

{-|
Given an 'Anchored' object containing the given list of anchors, rotate the
order of the given anchors clockwise by the given number of positions.

For example, given a diagram with positional anchors on it in these positions:

@
TL    T    TR

L          R

BL    B    BR
@

using @'rotatePosAnchors' 1 = 'rotateAnchors' (enumFrom AnchorL) 1@ will move
the anchors to these positions:

@
L     TL   T

BL         TR

B     BR   R
@

Using a parameter @n@ is equivalent to using @1@, @n@ times and a negative
number produces an anticlockwise rotation.

If any of the anchors do not exist, this function skips them.
-}
rotateAnchors :: (IsName anchor) => [anchor] -> Int -> Anchored t -> Anchored t
rotateAnchors allAnchorsList n t =
  let allAnchorsSet = HS.fromList . map toName $ allAnchorsList
      allObjAnchors = t ^. anchors
      presentAnchorsSet = (HS.fromMap . (() <$)) allObjAnchors `HS.intersection` allAnchorsSet
      presentAnchorsList = filter ((`HS.member` presentAnchorsSet) . toName) allAnchorsList
      rotateList k xs = drop k xs ++ take k xs
      rotatedList = rotateList ((-n) `mod` length presentAnchorsList) presentAnchorsList
      findOriginalPairing posAnch = fromJust $ HM.lookup (toName posAnch) allObjAnchors
      originalOffsets = map findOriginalPairing presentAnchorsList
      rotatedOffsets = zip (map toName rotatedList) originalOffsets
      newObjAnchors = HM.fromList rotatedOffsets `HM.union` allObjAnchors
  in t & anchors .~ newObjAnchors

-- | As 'rotateAnchors', but specialised to the list of all 'PositionalAnchor's.
rotatePosAnchors :: Int -> Anchored t -> Anchored t
rotatePosAnchors = rotateAnchors (enumFrom AnchorL)

--------------------------------------------------------------------------------
--  Qualifying Anchors
--------------------------------------------------------------------------------

instance Qualifiable t => Qualifiable (Anchored t) where
  (.>>) nm =
    (currentAnchor . _Just %~ (nm .>)) .
    (anchors %~ HM.fromList . over (mapped . _1) (nm .>) . HM.toList) .
    (anchoredObj %~ (nm .>>))

--------------------------------------------------------------------------------
--  Easily concatenate many anchored objects
--------------------------------------------------------------------------------

{-|
Starting from a base anchored object, recursively concatenate more objects to
the structure built up so far. Be sure to qualify anchors in the input so that
names aren't overwritten.

In each @(thatAnchor, thisAnchor, obj)@ triple, @thatAnchor@ refers to the
anchor point in the structure already constructed, and @thisAnchor@ refers to
the anchor point in the new object being added.
-}
anchorMany
  :: (Num (N t), Semigroup t, Additive (V t), HasOrigin t,
      IsName anchor) =>
     Anchored t -> [(anchor, anchor, Anchored t)] -> Anchored t
anchorMany = foldl' go
  where
    go base (thatAnch, thisAnch, obj)
      = alignAnchor thatAnch base <> alignAnchor thisAnch obj

-- | As 'anchorMany', but call 'unanchor' on the result. Convenient when you're
-- not going to be doing any more alignment using anchors with the result.
anchorMany_
  :: (Num (N c), Semigroup c, Additive (V c), HasOrigin c,
      IsName anchor) =>
     Anchored c -> [(anchor, anchor, Anchored c)] -> c
anchorMany_ base = unanchor . anchorMany base

--------------------------------------------------------------------------------
--  Debugging
--------------------------------------------------------------------------------

-- | Show a particular anchor in the 'Anchored' object.
showAnchor
  :: (Monoid m, Semigroup m, IsName a) =>
     a -> Anchored (QDiagram V2 Double m) -> Anchored (QDiagram V2 Double m)
showAnchor anch = moveFromAnchor . over anchoredObj showOrigin . moveToAnchor
  where
    moveToAnchor   t = t & anchoredObj %~ moveOriginBy ( getAnchorOffset anch t)
    moveFromAnchor t = t & anchoredObj %~ moveOriginBy (-getAnchorOffset anch t)

-- | Show a particular anchor in the 'Anchored' object, then 'unanchor'.
showAnchor_
  :: (Monoid m, Semigroup m, IsName a) =>
     a -> Anchored (QDiagram V2 Double m) -> QDiagram V2 Double m
showAnchor_ anch = unanchor . showAnchor anch
