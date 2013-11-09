{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Text as T
import Control.Lens hiding ((#), at, from, to)
import Text.Parsec as P
import Text.Parsec.Text

import Diagrams.Prelude hiding (option, (<>), hcat, text, (~~), render)
import Diagrams.Located
import Diagrams.Backend.SVG

import Diagrams.TwoD.Path.Metafont as MF
import Diagrams.TwoD.Path.Metafont.Types

import Text.PrettyPrint hiding (($$))
import Test.QuickCheck
import System.Process
import System.IO
import System.IO.Unsafe

import qualified Data.ByteString.Lazy as BS
import Data.Conduit
import Data.Conduit.Binary hiding (head)
import Data.Conduit.Text
import Data.Conduit.Process

------------------------------------------------------------
-- Print MF Paths in MF string syntax

class ToMF a where
  toMF :: a -> Doc

dots = text ".."

-- mf doesn't like coördinates in exponential notation
instance ToMF Double where
    toMF d = text $ integral ++ decimal where
      integral = show $ truncate d
      l = length integral
      n = max 0 (8-l) -- number of digits past decimal
      fracPart = abs d - (realToFrac . truncate . abs $ d)
      decimal =
          if n > 0
          then '.' : (show . truncate $ 10^n * fracPart)
          else ""

instance ToMF MFP where
    toMF (MFP _ [])  = empty
    toMF (MFP False [s]) = toMF s <> toMF (s^.x2)
    toMF (MFP True  [s]) = toMF s <> toMF (s^.x2) <> text "..cycle"
    toMF (MFP l (s0:ss)) = toMF s0 <> toMF (MFP l ss)

instance ToMF  (MetafontSegment (Maybe PathDir) BasicJoin) where
    toMF s = hcat [ toMF $ s^.x1, toMF $ s^.pj ]

instance ToMF P2 where
    toMF p = parens $ toMF (p^._x) <> comma <> toMF (p^._y)

instance ToMF (PathJoin (Maybe PathDir) BasicJoin) where
    toMF pj = hcat [toMF $ pj^.d1, dots, toMF $ pj^.j, dots, toMF $ pj^.d2]

instance ToMF (Maybe PathDir) where
    toMF Nothing = empty
    toMF (Just (PathDirDir d))  = braces $ hcat [toMF $ d^._x, comma, toMF $ d^._y]
    toMF (Just (PathDirCurl c)) = braces $ text "curl" <+> toMF c

instance ToMF BasicJoin where
    toMF (Left  tj) = hcat [ text "tension ", toMF $ tj^.t1
                           , text " and ", toMF $ tj^.t2 ]
    toMF (Right cj) = hcat [ text "controls ", toMF $ cj^.c1
                           , text " and ", toMF $ cj^.c2 ]

instance ToMF Tension where
    toMF (TensionAmt     t) = toMF t
    toMF (TensionAtLeast t) = text "atleast" <+> toMF t


------------------------------------------------------------
    -- Approximate Comparison for Doubles, Points

class Approx a where
  (~~) :: a -> a -> Bool

--instance (Fractional a, Ord a) => Approx a where
instance Approx Double where
  (~~) a b = abs (a - b) < 0.001

instance Approx R2 where
    z1 ~~ z2 = (z1^._x) ~~ (z2^._x) && (z1^._y) ~~ (z2^._y)

instance Approx (Offset Closed R2) where
    OffsetClosed v0 ~~ OffsetClosed v1 = v0 ~~ v1

instance Approx (Segment Closed R2) where
    Linear o0 ~~ Linear o1 = o0 ~~ o1
    Cubic c0 d0 o0 ~~ Cubic c1 d1 o1 = c0 ~~ c1 && d0 ~~ d1 && o0 ~~ o1

instance Approx (Trail R2) where
    t0 ~~ t1 = and $ zipWith (~~) (trailSegments t0) (trailSegments t1)

instance (Approx a, Approx (V a), AdditiveGroup (V a)) => Approx (Located a) where
    a0 ~~ a1 = (loc a0 .-. origin) ~~ (loc a1 .-. origin) && unLoc a0 ~~ unLoc a1
------------------------------------------------------------
-- Arbitrary instances for Points, Paths

instance Arbitrary R2 where
    arbitrary = r2 <$> arbitrary

instance Arbitrary P2 where
    arbitrary = p2 <$> arbitrary

instance Arbitrary Tension where
    arbitrary = oneof [ TensionAmt <$> t
                      , TensionAtLeast <$> t ] where
      -- Knuth writes that the curvature equations are only well
      -- behaved for tension >= 3/4
      t = (+3/4) . getNonNegative <$> arbitrary

instance Arbitrary TensionJoin where
    arbitrary = TJ <$> arbitrary <*> arbitrary

instance Arbitrary ControlJoin where
    arbitrary = CJ <$> arbitrary <*> arbitrary

-- instance Arbitrary BasicJoin where
--     arbitrary = oneof [ Left <$> arbitrary, Right <$> arbitrary ]
instance (Arbitrary d, Arbitrary j) => Arbitrary (PathJoin d j) where
    arbitrary = PJ <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PathDir where
    arbitrary = oneof [ PathDirDir <$> arbitrary, PathDirCurl <$> c ] where
      c = getPositive <$> arbitrary

instance Arbitrary (PathJoin d j) => Arbitrary (MetafontSegment d j) where
    arbitrary = MFS <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (MetafontSegment d j) => Arbitrary (MFPath d j) where
    arbitrary = MFP <$> arbitrary <*> ss where
      ss = getNonEmpty <$> arbitrary

------------------------------------------------------------
-- Parsing metafont output

header :: Parser ()
header = return () <* string "This is METAFONT, Version 2.718281 (TeX Live 2013/Debian)\n**(/usr/share/texlive/texmf-dist/metafont/base/expr.mf\ngimme an expr: >> Path at line 5:\n"

parsePath :: Parser (Located (Trail R2))
parsePath = header *> concretePath

concretePath :: Parser (Located (Trail R2))
concretePath = flip at <$> lookAhead pt <*> (fromSegments <$> many (try segment))

segment :: Parser (Segment Closed R2)
segment = do
    p0 <- pt
    (Controls c1 c2) <- parseControls
    p1 <- pt
    pure $ bezier3 (c1 .-. p0) (c2 .-. p0) (p1 .-. p0)

-- taken from Diagrams.TwoD.Path.Metafont.Parser with modifications
parseControls :: Parser Controls
parseControls = do
  string "..controls" *> spaces
  z1 <- pt
  _ <- spaces *> string "and" <* spaces
  z2 <- pt
  _ <- spaces *> string ".."
  return $ Controls z1 z2

-- just a convenience for the Parser above
data Controls = Controls P2 P2

-- Functions below copied from Diagrams.TwoD.Path.Metafont.Parser
num :: Parser Double
num = read <$> float where
  sign = plus <|> minus <|> unsigned
  plus = P.char '+' *> unsigned
  minus = (:) <$> P.char '-' <*> unsigned
  unsigned = many1 digit
  decimal = option "" (try $ (:) <$> P.char '.' <*> unsigned)
  float = (++) <$> sign <*> decimal

xy :: Parser (Double, Double)
xy = do
  spaces
  x <- num
  spaces *> P.char ',' *> spaces
  y <- num
  spaces
  return (x,y)

pt :: Parser P2
pt = P.char '(' *> (p2 <$> xy) <* P.char ')'

------------------------------------------------------------
-- Interaction with external metafont process

runMF :: String -> IO T.Text
runMF s = do
    (inp, out, _, _) <- runInteractiveCommand "mf"
    hSetBuffering out LineBuffering
    hPutStrLn inp "expr"
    hPutStr inp s
    T.pack <$> hGetContents out
    -- runResourceT $ input $= mf $$ sink where
    --     input = yield (T.pack $ "expr\n" ++ s) $= encode utf8
    --     mf = conduitCmd "mf"
    --     output = decode utf8 $$ sinkText where
    --   sinkText = await >>= return . maybe T.empty id

unsafeMF :: String -> (Located (Trail R2))
unsafeMF s = case parse parsePath "" (unsafePerformIO (runMF s)) of
    Left e   -> error $ show e
    Right tr -> tr

-- QuickCheck Properties
matchesMF :: MFP -> Bool
matchesMF pth = fromPath pth ~~ (unsafeMF . render . toMF $ pth)
