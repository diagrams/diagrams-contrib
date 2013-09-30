{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagrams.TwoD.Path.Metafont.Types where

import Control.Lens hiding ((#))
import Diagrams.TwoD.Types

data PathJoin d j = PJ { _d1 :: d, _j :: j, _d2 :: d }
  deriving (Functor, Show)

makeLenses ''PathJoin

data PathDir
  = PathDirCurl Curl
  | PathDirDir  Dir
    deriving Show

isCurl :: PathDir -> Bool
isCurl (PathDirDir _) = False
isCurl (PathDirCurl _) = True

type Curl = Double
type Dir  = R2

type BasicJoin = Either TensionJoin ControlJoin

data Tension
  = TensionAmt Double
  | TensionAtLeast Double
  deriving Show

getTension :: Tension -> Double
getTension (TensionAmt t)     = t
getTension (TensionAtLeast t) = t

data TensionJoin = TJ { _t1 :: Tension, _t2 :: Tension }
                 deriving Show

data ControlJoin = CJ { _c1 :: P2, _c2 :: P2 }
                 deriving Show

makeLenses ''TensionJoin
makeLenses ''ControlJoin

data MetafontPath = MetafontPath Bool (MFPathData P2)
                                           
data P
data J

data MFPathData a where
  MFPathCycle:: MFPathData P
  MFPathEnd  :: P2 -> MFPathData P
  MFPathPt   :: P2 -> MFPathData J -> MFPathData P
  MFPathJoin :: PathJoin (Maybe PathDir) BasicJoin -> MFPathData P -> MFPathData J

data MetafontSegment d j = MFS { _x1 :: P2, _pj :: (PathJoin d j), _x2 :: P2 }
                         deriving (Functor, Show)

data MFPath d j = MFP { _loop :: Bool, _segs :: [MetafontSegment d j] }
                deriving Show

type MFP = MFPath (Maybe PathDir) BasicJoin

type MFS = MetafontSegment (Maybe PathDir) BasicJoin

makeLenses ''MetafontSegment
makeLenses ''MFPath
