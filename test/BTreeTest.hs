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
  t <- genTree 1000 0.1
  defaultMain (drawT t # centerXY # pad 1.1 # sized (Width 4))

------------------------------------------------------------
-- Critical Boltzmann generator for binary trees

genTreeCrit :: ReaderT Int (StateT Int (RandT StdGen Maybe)) (BTree ())
genTreeCrit = do
  r <- getRandom
  if r <= (1/2 :: Double)
    then atom >> return Empty
    else atom >> (BNode () <$> genTreeCrit <*> genTreeCrit)

atom :: ReaderT Int (StateT Int (RandT StdGen Maybe)) ()
atom = do
  targetSize <- ask
  curSize <- get
  when (curSize >= targetSize) mzero
  put (curSize + 1)

genOneTree :: Int -> Double -> IO (Maybe (BTree ()))
genOneTree size eps = do
  g <- newStdGen
  let sizeWiggle = floor $ fromIntegral size * eps
      maxSz = size + sizeWiggle
      minSz = size - sizeWiggle
  let mt = (evalRandT ?? g) . (runStateT ?? 0) . (runReaderT ?? maxSz) $ genTreeCrit
  case mt of
    Nothing -> return Nothing
    Just (t,sz) -> if sz >= minSz then return (Just t) else return Nothing

genTree' :: Int -> Double -> StateT Int IO (BTree ())
genTree' size eps = do
  mt <- liftIO (genOneTree size eps)
  modify (+1)
  case mt of
    Nothing -> genTree' size eps
    Just t  -> return t

genTree :: Int -> Double -> IO (BTree ())
genTree size eps = evalStateT (genTree' size eps) 0
