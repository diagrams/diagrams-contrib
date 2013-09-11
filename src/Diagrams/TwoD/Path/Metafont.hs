{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagrams.TwoD.Path.Metafont where

import Control.Lens

import Diagrams.Prelude hiding ((&))

import  Diagrams.TwoD.Path.Metafont.Types
import  Diagrams.TwoD.Path.Metafont.Internal
