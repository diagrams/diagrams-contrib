{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Diagrams.TwoD.Path.Metafont.Parser
       (metafontParser) where

import Control.Lens ((^.))

import Text.Parsec
import Text.Parsec.Text

import Diagrams.Prelude hiding (option)
import Diagrams.TwoD.Path.Metafont.Types

num :: (Num n, Read n) => Parser n
num = read <$> float where
  sign = plus <|> minus <|> unsigned
  plus = char '+' *> unsigned
  minus = (:) <$> char '-' <*> unsigned
  unsigned = many1 digit
  decimal = option "" (try $ (:) <$> char '.' <*> unsigned)
  float = (++) <$> sign <*> decimal

-- points

xy :: (Num n, Read n) => Parser (n, n)
xy = do
  spaces
  x <- num
  spaces *> char ',' *> spaces
  y <- num
  spaces
  return (x,y)

pt :: (Num n, Read n) => Parser (P2 n)
pt = char '(' *> (p2 <$> xy) <* char ')'

-- Joins

anyJoin :: (Num n, Read n) => Parser (PathJoin (Maybe (PathDir n)) (BasicJoin n))
anyJoin = straightJoin <|> do
  d1' <- optionMaybe pathDir
  j' <- tenseLine <|> dotsJoin
  d2' <- optionMaybe pathDir
  return $ PJ d1' j' d2'

dotsJoin :: (Num n, Read n) => Parser (BasicJoin n)
dotsJoin = string ".." *> spaces *>
           (boundedJoin <|>tensionJoin <|> controlJoin <|> plainJoin)

plainJoin :: (Num n, Read n) => Parser (BasicJoin n)
plainJoin = pure (Left $ TJ t1' t1') where
  t1' = TensionAmt 1

tensionJoin :: (Num n, Read n) => Parser (BasicJoin n)
tensionJoin = do
  string "tension"
  spaces
  t1' <- num
  t2' <- try (spaces *> string "and" *> spaces *> num <* spaces) <|> pure t1'
  string ".."
  return . Left $ TJ (TensionAmt t1') (TensionAmt t2')

controlJoin :: (Num n, Read n) => Parser (BasicJoin n)
controlJoin = do
  string "controls" *> spaces
  z1 <- pt
  spaces *> string "and" <* spaces
  z2 <- pt
  spaces *> string ".."
  return . Right $ CJ z1 z2

boundedJoin :: (Num n, Read n) => Parser (BasicJoin n)
boundedJoin = char '.' *> pure (Left $ TJ t t) where t = TensionAtLeast 1

straightJoin :: (Num n, Read n) => Parser (PathJoin (Maybe (PathDir n)) (BasicJoin n))
straightJoin = try (string "--" *> notFollowedBy (char '-')) *> pure (PJ c jj c)
  where
    c = Just $ PathDirCurl 1
    jj = Left $ TJ (TensionAmt 1) (TensionAmt 1)

tenseLine :: (Num n, Read n) => Parser (BasicJoin n)
-- 4096 is the constant 'infinity' in Plain MetaFont
tenseLine = string "---" *> pure (Left $ TJ t t) where t = TensionAmt 4096

-- Directions

dir :: (Num n, Read n) => Parser (PathDir n)
dir = PathDirDir . r2 <$> xy

curl :: (Num n, Read n) => Parser (PathDir n)
curl = PathDirCurl <$> (string "curl" *> spaces *> num)

pathDir :: (Num n, Read n) => Parser (PathDir n)
pathDir = do
  char '{' *> spaces
  d <- curl <|> dir
  spaces *> char '}'
  return d

-- Segments & Paths

mfs :: (Num n, Read n) => Parser (MetafontSegment (Maybe (PathDir n)) (BasicJoin n) n)
mfs = MFS <$> pt <*> anyJoin <*> lookAhead pt

matches :: Stream s m t => ParsecT s u m a -> ParsecT s u m Bool
matches p = option False (p *> return True)

-- | Parse a 'Text' value in Metafont syntax, as destribed in /The
-- METAFONTbook/.
metafontParser :: (Num n, Read n) => Parser (MFPath (Maybe (PathDir n)) (BasicJoin n) n)
metafontParser = do
  ss <- many1 (try mfs)
  lastP <- pt
  lastD <- optionMaybe pathDir
  c  <- matches $ string "..cycle"
  if c
     then return . MFP c $ ss ++ [MFS lastP (PJ lastD (Left $ TJ (TensionAmt 1) (TensionAmt 1)) Nothing) (head ss^.x1)]
    else return $ MFP c ss
