{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Layout.Wrap
-- Copyright   :  (c) 2012 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
--
-- An algorithm for filling space in a fashion akin to word-wrapping.
--
------------------------------------------------------------------------

module Diagrams.Layout.Wrap where

import           Control.Arrow    (first, (&&&))
import           Data.Foldable    (foldMap)
import           Data.List        (find, inits, tails)
import           Diagrams.Prelude hiding (start)
import           Linear.Epsilon

-- TODO: Take into account the negative bounds, and iteratively refine
--   the list selection.

-- TODO: Search for a region before / after the target pick.

-- | @wrapDiagram@ post-processes the results of @wrapOutside@ /
--   @wrapInside@ into a Diagram of the result.  This only works when
--   applying them to a list of diagrams.
wrapDiagram :: (HasLinearMap v, Metric v, OrderedField n)
            => ([(v n, QDiagram b v n Any)], [QDiagram b v n Any]) -> QDiagram b v n Any
wrapDiagram = foldMap (uncurry translate) . fst

-- | @wrapOutside@ is the same as @wrapInside@, but with an inverted
--   predicate.
wrapOutside :: ( Enveloped a, V a ~ v, N a ~ n
               , Metric v, OrderedField n, Epsilon n -- See [6.12.3] note below
               )
            => (Point v n -> Bool) -> [v n] -> Point v n -> [a] -> ([(v n, a)], [a])
wrapOutside f = wrapInside (not . f)

-- | @wrapInside@ greedily wraps content to fill a space defined by a
--   predicate.  It is passed a list of vectors which express the
--   order of dimensions to be filled.  In other words, wrapping RTL
--   text is done by passing in [unitX, unitY], to first exhaust
--   space horizontally, and then vertically.
--
--   Note that this function does not guarantee that there are not
--   points inside each positioned item for which the predicate is
--   False.  Instead, only the corners of the bounds, along each axii,
--   are used.
wrapInside :: forall a v n.
           ( Enveloped a, V a ~ v, N a ~ n
           , Metric v, OrderedField n, Epsilon n -- See [6.12.3] note below
           )
           => (Point v n -> Bool) -> [v n] -> Point v n
           -> [a] -> ([(v n, a)], [a])
wrapInside f axis start = rec zeros
 where
  zeros = map snd . zip axis $ repeat (0, 0)
  norms = map normalize axis
  getVector = sumV . zipWith (^*) norms

-- [[min bound, max bound]] of each axis.
  boundsScalars :: a -> [[v n]]
  boundsScalars d
    = flip map norms
    $ \v -> map (.-. origin) [envelopeP (negated v) d, envelopeP v d]

-- Recurses on the list of items to lay out, maintaing a current set of
-- coefficients for the different axii, each paired with the maximum
-- boundary seen in that direction.
  rec :: [(n, n)] -> [a] -> ([(v n, a)], [a])
  rec _ [] = (mempty, [])
  rec scs (d:ds)
-- Recurse a satisfactory position can be found, otherwise yields the
-- list of the remaining diagrams to be laid out.
    = maybe (mempty, d:ds)
            (\(v, scs') -> first ((v, d):) $ rec scs' ds)
    $ find (check . fst) potential
   where
    curB = boundsScalars d

-- Yields whether a given vector offset avoids collision.
    check v = all (f . (start .+^) . sumV . (v:)) $ sequence curB

-- Updates the max bounds of an axis.
    maxB [_, b] (x, m) = (x, max m $ x + norm b)
    maxB _ _ = error "Diagrams.Layout.Wrap.wrapInside:maxB: pattern-match failure.  Please report this as a bug."

-- List of potential offsets to try, each paired with an updated list
-- of current / maxbound scalar coefficients for the axis.
    potential = map (getVector . map fst &&& zipWith maxB curB)
-- Try setting an axis to its max-seen bound, zeroing all preceding.
              . zipWith (++) (inits $ repeat (0, 0))
              . map dupFirstY
              . init $ tails scs

    dupFirstY ((_,x):xs) = (x,x):xs
    dupFirstY _          = error "Diagrams.Layout.Wrap.wrapInside:dupFirstY: pattern-match failure.  Please report this as a bug."

-- [6.12.3]: It should be possible to infer the Metric v and
--   OrderedField n constraints from Enveloped a, v ~ V a,
--   but GHC 6.12.3 cannot, so we redundantly list them here to
--   preserve support for 6.12.3.
-- TODO this comment is obsolete; we certainly do not support GHC 6.12.3

--   Attempt at diagrams-haddock example, but I don't understand how Wrap works
--
--   > import Diagrams.Layout.Wrap
--   > import Control.Arrow (first)
--   > wrapInsideEx = position ((map . first $ (origin .+^)) ds)
--   >   where (ds,_) = wrapInside
--   >                    (getAny . (runQuery . query $ (circle 15 :: D V2 n)))
--   >                    [unitX, unitY]
--   >                    (origin)
--   >                    (repeat (circle 1 # fc black))
--
--   <diagrams/wrapInsideEx.svg#diagram=wrapInsideEx&width=200>
