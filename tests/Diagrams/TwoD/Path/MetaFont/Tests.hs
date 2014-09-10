{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Data.Text (Text)
import System.FilePath
import Control.Arrow (second)
import Control.Lens hiding ((#), at, from, to)
import Text.Parsec

import Diagrams.Prelude hiding (option)
import Diagrams.Backend.SVG

import Diagrams.TwoD.Path.Metafont as MF
import Diagrams.TwoD.Path.Metafont.Types

litTests :: [(FilePath, Text)]
litTests = [
    ("p15a", "(0,0)..(0,1)..(1,1)..(2,0)"),
    ("p15b", "(1,0)..(0,0)..(0,1)..(2,1)..(2,0)..(1,0)"),
    ("p15c", "(1,0)..(0,0)..(0,1)..(2,1)..(2,0)..cycle"),
    ("p15d", "(1,0)..(0,0)..(0,1)..tension 1.2..(2,1)..(2,0)..cycle"),
    ("p16a", "(1,0)..(0,0)..tension 1 and 1.5..(0,1)..tension 1.5 and 1..(2,1)..(2,0)..cycle"),
    ("p16b", "(1,0)..(0,0){-1,0}..(0,1)..(2,1)..(2,0){-1,0}..cycle"),
    ("p17a", "(1,0)..(0,0){-1,-1}..(0,1)..(2,1)..(2,0){-1,1}..cycle"),
    ("p17b", "(0,0){curl0}..(1,1){2,1}..{curl0}(2,1)"),
    ("p17c", "(0,0){curl2}..(1,1){2,1}..{curl2}(2,1)"),
    ("p19a", "(0,0)..controls (0,1) and (1,1)..(2,0)"),
    ("p127a", "(0,1)..(0.5,0)---(1.8,0)...{0,1}(2,1)"),
    ("p127b", "(0,1)..(0.5,0)--(1.8,0)...{0,1}(2,1)"),
    ("p127c", "(0,1)...(0.5,0)---(1.8,0)..{0,1}(2,1)")
    ]

astTests :: [(FilePath, Trail V2 n)]
astTests = [("p125", let -- this is Knuth's branch4
                z1 = p2 (0,509)
                z2 = p2 (-14,492)
                z3 = p2 (-32,481)
                z4 = p2 (-42,455)
                z5 = p2 (-62,430)
                z6 = p2 (-20,450)
                z7 = p2 (42,448)
                z8 = p2 (38,465)
                z9 = p2 (4,493)
                in
                 mconcat [MF.flex [z1, z2, z3], MF.flex [z3, z4, z5],
                          MF.flex [z5, z6, z7], MF.flex [z7, z8, z9, z1]])
           ]

combinatorTests :: [(FilePath, Trail V2 n)]
combinatorTests = map (second metafont)
                  [ ("p15a_", z4.--.z1.--.z2.--.endpt z6)
                  , ("p15b_", z5.--.z4.--.z1.--.z3.--.z6.--.endpt z5)
                  , ("p15c_", z5.--.z4.--.z1.--.z3.--.z6.--.cyclePath)
                  , ("p15d_", z5.--.z4.--.z1.-tension 1.2-.z3.--.z6.--.cyclePath)
                  , ("p16a_", z5.--.z4.-tensions 1 1.5-.z1.-tensions 1.5 1-.z3.--.z6.--.cyclePath)
                  , ("p16b_", z5.--.z4.-goingLeft-.z1.--.z3.--.z6.-goingLeft-.cyclePath)
                  ] where
  z1 = p2 (0,1)
  z2 = p2 (1,1)
  z3 = p2 (2,1)
  z4 = p2 (0,0)
  z5 = p2 (1,0)
  z6 = p2 (2,0)
  goingLeft = leaving unit_X

boundedTests :: [(FilePath, Diagram SVG V2 n)]
boundedTests = map (second (mconcat . map (metafont)))
               [ ("p18a", demo mempty)
               , ("p18b", map (withJoin mempty . Deg) [0,10..90])
               , ("p18c", demo bounded)
               ]
  where
    bounded = simpleJoin & j .~ (Just . Left $ TJ (TensionAtLeast 1) (TensionAtLeast 1))
    left = leaving (fromDirection . Deg $ 60)
    demo jj = map (withJoin jj . Deg) [0,(-10)..(-120)]
    withJoin jj d = origin .- left <> jj <> arriving (fromDirection d) -. (endpt $ p2 (6,0))

illustrateSegment :: FixedSegment V2 n -> Diagram SVG V2 n
illustrateSegment (FLinear from to) = position [
  (from, ptMark # fc blue),
  (to,   ptMark # fc blue)]
illustrateSegment (FCubic from z1 z2 to) = position [
  (z1, ptMark # fc red),
  (z2, ptMark # fc red)] <> illustrateSegment (FLinear from to)

ptMark :: Diagram SVG V2 n
ptMark = circle 0.02 # lw 0

illustrateTrailCtls :: Trail V2 n -> Diagram SVG V2 n
illustrateTrailCtls tr = strokeTrail tr <> (mconcat . map illustrateSegment . fixTrail . flip at origin $ tr)

refPts :: Diagram SVG V2 n
refPts = position $ zip (map p2 $ (,) <$> [0,1,2] <*> [0,1]) (repeat c) where
  c = circle 0.02 # fc blue # lw 0

toSVG :: (FilePath,Trail V2 n) -> IO ()
toSVG (fn,tr) = do
  renderSVG (replaceExtension fn "svg") (Width 400) $
    illustrateTrailCtls tr

renderMF :: (FilePath, Text) -> IO ()
renderMF (fn, p) = do
  print fn
  case MF.fromString p of
    Left err -> print err
    Right tr -> toSVG (fn,tr)

main :: IO ()
main = do
    mapM_ renderMF litTests
    mapM_ toSVG (astTests ++ combinatorTests)
    mapM_ (\(fn, d) -> renderSVG (replaceExtension fn "svg") (Width 400) d) boundedTests
