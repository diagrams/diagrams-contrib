{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Diagrams.TwoD.Path.Metafont.Parser
       (metafontParser) where

import Control.Lens ((^.))

import Text.Parsec
import Text.Parsec.Text

import Diagrams.Prelude hiding (option)
import Diagrams.TwoD.Path.Metafont.Types

num :: Parser Double
num = read <$> float where
  sign = plus <|> minus <|> unsigned
  plus = char '+' *> unsigned
  minus = (:) <$> char '-' <*> unsigned
  unsigned = many1 digit
  decimal = option "" (try $ (:) <$> char '.' <*> unsigned)
  float = (++) <$> sign <*> decimal

-- points

xy :: Parser (Double, Double)
xy = do
  spaces
  x <- num
  spaces *> char ',' *> spaces
  y <- num
  spaces
  return (x,y)

pt :: Parser P2
pt = char '(' *> (p2 <$> xy) <* char ')'

-- Joins

join :: Parser (PathJoin (Maybe PathDir) BasicJoin)
join = straightJoin <|> do
  d1' <- optionMaybe pathDir
  j' <- tenseLine <|> dotsJoin
  d2' <- optionMaybe pathDir
  return $ PJ d1' j' d2'

dotsJoin :: Parser BasicJoin
dotsJoin = string ".." *> spaces *>
           (boundedJoin <|>tensionJoin <|> controlJoin <|> plainJoin)

plainJoin :: Parser BasicJoin
plainJoin = pure (Left $ TJ t1' t1') where
  t1' = TensionAmt 1

tensionJoin :: Parser BasicJoin
tensionJoin = do
  string "tension"
  spaces
  t1' <- num
  t2' <- try (spaces *> string "and" *> spaces *> num <* spaces) <|> pure t1'
  string ".."
  return . Left $ TJ (TensionAmt t1') (TensionAmt t2')

controlJoin :: Parser BasicJoin
controlJoin = do
  string "controls" *> spaces
  z1 <- pt
  spaces *> string "and" <* spaces
  z2 <- pt
  spaces *> string ".."
  return . Right $ CJ z1 z2

boundedJoin :: Parser BasicJoin
boundedJoin = char '.' *> pure (Left $ TJ t t) where t = TensionAtLeast 1

straightJoin :: Parser (PathJoin (Maybe PathDir) BasicJoin)
straightJoin = try (string "--" *> notFollowedBy (char '-')) *> pure (PJ c jj c)
  where
    c = Just $ PathDirCurl 1
    jj = Left $ TJ (TensionAmt 1) (TensionAmt 1)

tenseLine :: Parser BasicJoin
-- 4096 is the constant 'infinity' in Plain MetaFont
tenseLine = string "---" *> pure (Left $ TJ t t) where t = TensionAmt 4096

-- Directions

dir :: Parser PathDir
dir = PathDirDir . r2 <$> xy

curl :: Parser PathDir
curl = PathDirCurl <$> (string "curl" *> spaces *> num)

pathDir :: Parser PathDir
pathDir = do
  char '{' *> spaces
  d <- curl <|> dir
  spaces *> char '}'
  return d

-- Segments & Paths

mfs :: Parser (MetafontSegment (Maybe PathDir) BasicJoin)
mfs = MFS <$> pt <*> join <*> lookAhead pt

matches :: Stream s m t => ParsecT s u m a -> ParsecT s u m Bool
matches p = option False (p *> return True)

metafontParser :: Parser (MFPath (Maybe PathDir) BasicJoin)
metafontParser = do
  ss <- many1 (try mfs)
  lastP <- pt
  lastD <- optionMaybe pathDir
  c  <- matches $ string "..cycle"
  if c
     then return . MFP c $ ss ++ [MFS lastP (PJ lastD (Left $ TJ (TensionAmt 1) (TensionAmt 1)) Nothing) (head ss^.x1)]
    else return $ MFP c ss
