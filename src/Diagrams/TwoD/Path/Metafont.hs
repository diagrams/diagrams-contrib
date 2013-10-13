{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Metafont allows declaring Diagrams Paths using Donald Knuth's MetaFont syntax.
--  This module is intended to be imported qualified.
module Diagrams.TwoD.Path.Metafont
       (
             module  Diagrams.TwoD.Path.Metafont.Combinators
           , module  Diagrams.TwoD.Path.Metafont
           , metafontParser
       )
       where

import Control.Lens hiding ((#), at)
import Data.Text (Text)
import Data.Either
import Text.Parsec (ParseError, parse)

import Diagrams.Prelude hiding (view, over)

import Diagrams.TwoD.Path.Metafont.Types
import Diagrams.TwoD.Path.Metafont.Internal
import Diagrams.TwoD.Path.Metafont.Combinators
import Diagrams.TwoD.Path.Metafont.Parser

-- | MF.fromString is the primary interface to the MetaFont library.
--  It takes a ByteString in MetaFont syntax, and attempts to return a
--  TrailLike.
fromString :: (TrailLike t, V t ~ R2) => Text -> Either ParseError t
fromString s = case parse metafontParser "" s of
  (Left err) -> Left err -- with different type
  (Right p)  -> Right . fromPath  $ p

-- | fromStrings takes a list of MetaFont strings, and returns either
--  all errors, or, if there are no parsing errors, a TrailLike for
--  each string.  fromStrings is provided as a convenience because the
--  MetaFont &-join is not supported.  mconcat on the TrailLike is
--  equivalent, with clearer semantics.
fromStrings :: (TrailLike t, V t ~ R2) => [Text] -> Either [ParseError] [t]
fromStrings ss = case partitionEithers . map fromString $ ss of
  ([],ts) -> Right ts
  (es,_)  -> Left es

-- | Should you wish to construct the MFPath in some other fashion,
--   fromPath makes a TrailLike directly from the MFPath
fromPath :: (TrailLike t, V t ~ R2) => MFP -> t
fromPath = trailLike . locatedTrail . over (segs.mapped) computeControls . solve

-- | flex ps draws a Trail through the points ps, such that at every
-- point p ∊ ps except the endpoints, the Trail is parallel to the
-- line from the first to the last point.  This is the same as the
-- flex command defined in plain MetaFont.
flex :: (TrailLike t, V t ~ R2) => [P2] -> t
flex ps = fromPath . MFP False $ (s0:rest) where
  tj = (Left (TJ (TensionAmt 1) (TensionAmt 1)))
  jj = PJ Nothing tj Nothing
  s0 = MFS (head ps) jj (head.tail $ ps)
  d = Just . PathDirDir $ (last ps) .-. (head ps)
  seg z1 z2 = MFS z1 (PJ d tj Nothing) z2
  rest = zipWith seg (init . tail $ ps) (tail . tail $ ps)

-- | metafont converts a path defined in the Metafont combinator synax into a
-- native Diagrams TrailLike.
metafont :: (TrailLike t, V t ~ R2) => MFPathData P -> t
metafont = fromPath . mfPathToSegments
