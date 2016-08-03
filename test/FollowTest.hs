{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Diagrams.TwoD.Path.Follow

wibble :: Trail' Line V2 Double
wibble = hrule 1 <> hrule 0.5 # rotateBy (1/6) <> hrule 0.5 # rotateBy (-1/6) <> a
  where a = arc (xDir # rotateBy (-1/4)) (1/5 @@ turn)
          # scale 0.7

main = defaultMain $
  [ wibble
  , wibble
    # replicate 5
    # ala follow foldMap
  ]
  # map stroke
  # map centerXY
  # vsep 1
  # frame 0.5
