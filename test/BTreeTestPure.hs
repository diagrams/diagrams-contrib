{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Applicative
import           Control.Lens                   hiding (( # ))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Diagrams.TwoD.Layout.Tree

t = BNode () (BNode () (leaf ()) (leaf ())) (BNode () Empty (BNode () (leaf ()) Empty))

fact n = product [1..n]
binom n k = fact n `div` (fact k * fact (n-k))

-- sierpinskiTree n k
--  |
--  where b = binom n k

drawT = maybe mempty (renderTree (const (circle 0.05 # fc black)) (~~))
      . symmLayoutBin' with { slVSep = 0.5 }

main = do
  let t = genTree 500 0.05
  defaultMain (drawT t # centerXY # pad 1.1 # sized (Width 4))

------------------------------------------------------------
-- Critical Boltzmann generator for binary trees

genTreeCrit :: ReaderT Int (StateT Int (RandT StdGen Maybe)) (BTree ())
genTreeCrit = do
  r <- getRandom
  if r <= (1/2 :: Double)
    then return Empty
    else atom >> (BNode () <$> genTreeCrit <*> genTreeCrit)

atom :: ReaderT Int (StateT Int (RandT StdGen Maybe)) ()
atom = do
  targetSize <- ask
  curSize <- get
  when (curSize >= targetSize) mzero
  put (curSize + 1)

genOneTree :: Int -> Int -> Double -> Maybe (BTree ())
genOneTree seed size eps =
  case mt of
    Nothing -> Nothing
    Just (t,sz) -> if sz >= minSz then Just t else Nothing

  where
    g          = mkStdGen seed
    sizeWiggle = floor $ fromIntegral size * eps
    maxSz = size + sizeWiggle
    minSz = size - sizeWiggle
    mt = (evalRandT ?? g) . (runStateT ?? 0) . (runReaderT ?? maxSz) $ genTreeCrit

genTree' :: Int -> Int -> Double -> State Int (BTree ())
genTree' seed size eps = do
  let mt = genOneTree seed size eps
  modify (+1)
  case mt of
    Nothing -> genTree' (seed+1) size eps
    Just t  -> return t

genTree :: Int -> Double -> BTree ()
genTree size eps = evalState (genTree' 0 size eps) 0
