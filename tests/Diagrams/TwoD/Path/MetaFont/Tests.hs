{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import System.FilePath

import Diagrams.Prelude hiding (option)
import Diagrams.Backend.SVG

import Diagrams.TwoD.Path.Metafont as MF

litTests :: [(FilePath, Text)]
litTests = [("p15a",  "(0,0)..(0,1)..(1,1)..(2,0)"),
         ("p15b", "(1,0)..(0,0)..(0,1)..(2,1)..(2,0)..(1,0)"),
         ("p15c",   "(1,0)..(0,0)..(0,1)..(2,1)..(2,0)..cycle"),
         ("p15d",   "(1,0)..(0,0)..(0,1)..tension 1.2..(2,1)..(2,0)..cycle"),
         ("p16a",      "(1,0)..(0,0)..tension 1 and 1.5..(0,1)..tension 1.5 and 1..(2,1)..(2,0)..cycle"),
         ("p16b",   "(1,0)..(0,0){-1,0}..(0,1)..(2,1)..(2,0){-1,0}..cycle"),
         ("p17a",      "(1,0)..(0,0){-1,-1}..(0,1)..(2,1)..(2,0){-1,1}..cycle"),
         ("p17b", "(0,0){curl0}..(1,1){2,1}..{curl0}(2,1)"),
         ("p17c", "(0,0){curl2}..(1,1){2,1}..{curl2}(2,1)"),
         ("p19a", "(0,0)..controls (0,1) and (1,1)..(2,0)")
         ]

astTests :: [(FilePath, Trail R2)]
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

illustrateSegment :: FixedSegment R2 -> Diagram SVG R2
illustrateSegment (FLinear start end) = position [
  (start, ptMark # fc blue),
  (end,   ptMark # fc blue)]
illustrateSegment (FCubic start c1 c2 end) = position [
  (c1, ptMark # fc red),
  (c2, ptMark # fc red)] <> illustrateSegment (FLinear start end)

ptMark :: Diagram SVG R2
ptMark = circle 0.02 # lw 0

illustrateTrailCtls :: Trail R2 -> Diagram SVG R2
illustrateTrailCtls = mconcat . map illustrateSegment . fixTrail . flip at origin

refPts :: Diagram SVG R2
refPts = position $ zipWith (,) (map p2 $ (,) <$> [0,1,2] <*> [0,1]) (repeat c) where
  c = circle 0.02 # fc blue # lw 0

toSVG :: (FilePath,Trail R2) -> IO ()
toSVG (fn,tr) = do
  renderSVG (replaceExtension fn "svg") (Width 400) $
    strokeTrail tr <> illustrateTrailCtls tr

renderMF :: (FilePath, Text) -> IO ()
renderMF (fn, p) = do
  print fn
  case MF.fromString p of
    Left err -> print err
    Right tr -> toSVG (fn,tr)

main :: IO ()
main = mapM_ renderMF litTests >> mapM_ toSVG astTests
