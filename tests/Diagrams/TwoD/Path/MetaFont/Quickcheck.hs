{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified Data.Text as T
import Control.Lens hiding ((#), at, from)
import Text.Parsec as P
import Text.Parsec.Text

import Diagrams.Prelude hiding (option, (<>), hcat, text, (~~), render)
import Diagrams.Coordinates
import Diagrams.Located
import Diagrams.Backend.SVG
import Data.AffineSpace.Point (unPoint)

import Diagrams.TwoD.Path.Metafont as MF
import Diagrams.TwoD.Path.Metafont.Types
import Diagrams.TwoD.Path.Metafont.Internal

import Text.PrettyPrint hiding (($$))
import Test.QuickCheck
import Control.DeepSeq
import System.Process
import System.IO
import System.IO.Unsafe

import Data.Maybe
import qualified Data.ByteString.Lazy as BS
-- import Data.Conduit
-- import Data.Conduit.Binary hiding (head)
-- import Data.Conduit.Text
-- import Data.Conduit.Process

------------------------------------------------------------
-- Print MF Paths in MF string syntax

class ToMF a where
  toMF :: a -> Doc

dots = text ".."

-- mf doesn't like coördinates in exponential notation
instance ToMF Double where
    toMF d = text $ sign ++ integral ++ decimal where
      integral = show . truncate . abs $ d
      sign = if d < 0 then "-" else ""
      l = length integral
      n = max 0 (8-l) -- number of digits past decimal
      fracPart = abs d - (realToFrac . truncate . abs $ d)
      decimal = if n > 0
                then '.' : (zeros ++ digits)
                else ""
        where
          digits = show . truncate $ 10^n * fracPart
          zeros = replicate (n - length digits) '0'

instance ToMF MFP where
    toMF (MFP _ [])  = empty
    toMF (MFP _ [s]) = toMF s <> toMF (s^.x2)
    toMF (MFP l (s0:ss)) = toMF s0 <> toMF (MFP l ss)

instance ToMF  (MetafontSegment (Maybe PathDir) BasicJoin) where
    toMF s = hcat [ toMF $ s^.x1, toMF $ s^.pj ]

instance ToMF P2 where
    toMF p = parens $ toMF (p^._x) <> comma <> toMF (p^._y)

instance ToMF (PathJoin (Maybe PathDir) BasicJoin) where
    toMF pj@(PJ _ (Left _) _) =
        hcat [toMF $ pj^.d1, dots, toMF $ pj^.j, dots, toMF $ pj^.d2]
    toMF pj = hcat [dots, toMF $ pj^.j, dots]


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

epsilon = 0.001

class Approx a where
  (~~) :: a -> a -> Bool

--instance (Fractional a, Ord a) => Approx a where
instance Approx Double where
  (~~) a b = abs (a - b) / abs a < epsilon

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
    arbitrary = (^&) <$> choose (-4095,4095) <*> choose (-4095,4095)

instance Arbitrary P2 where
    arbitrary = (^&) <$> choose (-4095,4095) <*> choose (-4095,4095)

instance Arbitrary Tension where
    arbitrary = oneof [ TensionAmt <$> choose(0.75,4095)
                      , TensionAtLeast <$> choose(0.75,4095) ] where
      -- Knuth writes that the curvature equations are only well
      -- behaved for tension >= 3/4

instance Arbitrary TensionJoin where
    arbitrary = TJ <$> arbitrary <*> arbitrary

instance Arbitrary ControlJoin where
    arbitrary = CJ <$> arbitrary <*> arbitrary

instance (Arbitrary d, Arbitrary j) => Arbitrary (PathJoin d j) where
    arbitrary = PJ <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PathDir where
    arbitrary = oneof [ PathDirDir <$> arbitrary, PathDirCurl <$> c ] where
      c = choose (0, 4095)

instance Arbitrary (PathJoin d j) => Arbitrary (MetafontSegment d j) where
    arbitrary = MFS <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (MetafontSegment d j) => Arbitrary (MFPath d j) where
    arbitrary = oneof [ MFP False <$> (contiguous <$> ss)
                      , MFP True  <$> (connectLoop . contiguous) <$> ss'
                      ]
                        where
      ss = listOf1 arbitrary
      ss' = listOf1 arbitrary `suchThat` ((>=2) . length)
      -- make first point of each segment match last point of preceding
      contiguous (s:ss) = go (s^.x1) (s:ss)
      go _ [] = []
      go p (s:ss) = (s & x1 .~ p):go (s^.x2) ss
      connectLoop ss = ss & _last.x2 .~ (ss^?!_head.x1)

------------------------------------------------------------
-- NFData instances for Paths, all trivial

instance NFData R2

instance NFData v => NFData (Point v) where
    rnf p = rnf $ unPoint p

instance NFData PathDir where
    rnf (PathDirCurl c) = rnf c
    rnf (PathDirDir  d) = rnf d

instance NFData Tension where
    rnf (TensionAmt     t) = rnf t
    rnf (TensionAtLeast t) = rnf t

instance NFData TensionJoin where
    rnf tj = rnf (tj^.t1) `seq` rnf (tj^.t2)

instance NFData ControlJoin where
    rnf cj = rnf (cj^.c1) `seq` rnf (cj^.c2)

instance (NFData d, NFData j) => NFData (PathJoin d j) where
    rnf jj = rnf (jj^.d1) `seq` rnf (jj^.j) `seq` rnf (jj^.d2)

instance (NFData d, NFData j) => NFData (MetafontSegment d j) where
    rnf s = rnf (s^.x1) `seq` rnf (s^.pj) `seq` rnf (s^.x2)

instance (NFData d, NFData j) => NFData (MFPath d j) where
    rnf pth = rnf (pth^.loop) `seq` rnf (pth^.segs)

instance NFData v => NFData (Offset Closed v) where
    rnf (OffsetClosed v) = rnf v

instance NFData v => NFData (Segment Closed v) where
    rnf (Linear v) = rnf v
    rnf (Cubic c d o) = rnf c `seq` rnf d `seq` rnf o

-- TODO Is this right?
instance (InnerSpace v, OrderedField (Scalar v), NFData v) => NFData (Trail v) where
    rnf = rnf . trailSegments

instance (NFData a, NFData (V a)) => NFData (Located a) where
    rnf l = rnf (loc l) `seq` rnf (unLoc l)
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
    p1 <- lookAhead pt
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
  -- Metafont is perfectly willing to put newlines in the middle of numbers
  decimal = optional (P.char '\n') *> option "" (try $ (:) <$> P.char '.' <*> unsigned)
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

noExceptions :: MFP -> Bool
noExceptions pth = (fromPath pth :: Located (Trail R2)) `deepseq` True

------------------------------------------------------------
-- Helpers for drawing segments

illustrateSegment :: FixedSegment R2 -> Diagram SVG R2
illustrateSegment (FLinear from to) = position [
  (from, ptMark # fc blue),
  (to,   ptMark # fc blue)]
illustrateSegment (FCubic from z1 z2 to) = position [
  (z1, ptMark # fc red),
  (z2, ptMark # fc red)] `mappend` (illustrateSegment (FLinear from to))

ptMark :: Diagram SVG R2
ptMark = circle 0.02 # lw 0

illustrateTrailCtls :: Trail R2 -> Diagram SVG R2
illustrateTrailCtls tr = (strokeTrail tr) `mappend` (mconcat . map illustrateSegment . fixTrail . flip at origin $ tr)

hVersion :: MFP -> Diagram SVG R2
hVersion = illustrateTrailCtls . fromPath

mfVersion :: MFP -> Diagram SVG R2
mfVersion = illustrateTrailCtls . unLoc . unsafeMF . render . toMF

compareMF :: MFP -> IO ()
compareMF p = renderSVG "mfTests.svg" (Dims 600 600) $ (hVersion p # lc red) `mappend` (mfVersion p # lc green)

curlTest = renderSVG "curlTest.svg" (Dims 600 600) . mconcat . map cTest $ [1,2,4,8]
  where
    cTest c = lw (c/100) . metafont $ origin .- (mempty & d1 .~ (Just . PathDirCurl $ c) & d2 .~ (Just . PathDirDir $ (1 ^& (-1)))) -. endpt (10 ^& 0)
