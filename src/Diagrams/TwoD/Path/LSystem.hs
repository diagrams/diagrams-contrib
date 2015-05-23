{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.LSystem
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Create LSystem diagrams and paths.
--
-- See "The Algorithmic Beauty of Plants".
-- <http://algorithmicbotany.org/papers/abop/abop-ch1.pdf>
-------------------------------------------------------------------------------

module Diagrams.TwoD.Path.LSystem
  ( -- * Lindenmayer Systems

    -- ** L-Systems
    Symbol(..)
  , Rules
  , generations

    -- ** L-System graphics
  , lSystemR , lSystem , lSystemPath , lSystemDiagram

    -- ** Making rules from strings
  , symbol, symbols
  , rule

    -- * Predefined L-Systems
  , sierpinski, cantor
  , dragon, hexGosper, kochIsland, kochLake
  , koch1, koch2, koch3, koch4, koch5, koch6
  , tree1, tree2, tree3, tree4, tree5, tree6

    -- * Re-exports from "Diagrams.TwoD.Path.Turtle"
  , TurtleState
  , getTurtlePath, getTurtleDiagram
  ) where

import           Control.Monad.Reader
import           Diagrams.Prelude                    hiding (local)
import           Diagrams.TwoD.Path.Turtle.Internal
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)

-- | Symbols:
--
--     * @F (\'F\') draw a segment.@
--     * @G (\'f\' or \'G\') move the turtle one unit without drawing anything.@
--     * @Plus (\'+\') turn the turtle clockwise.@
--     * @Minus (\'-\') turn the turtle anti-clockwise.@
--     * @Reverse (\'!\') turn the turtle 180 degrees.@
--     * @Push (\'[\') push the current state onto the stack.@
--     * @Pop (\']\') pop the current state.@
--     * @Width x (\'\<\', \'\>\') increase (decrease) stroke width by factor of 1.1 (0.9).@
--     * @Delta x (\'\(\', \'\)\') increase (decrease) turn angle by factor of 1.1 (0.9).@
--     * @X n (\'X\',\'Y\',\'Z\',\'A\',\'B\') constants.@
data Symbol n
  = F
  | G
  | Plus
  | Minus
  | Reverse
  | Push
  | Pop
  | X Int
  | Width n
  | Delta n
  deriving (Eq, Ord, Show)

-- | Production rules.
type Rules n = Map (Symbol n) [Symbol n]

data Environment n = Environment
  { angleInc    :: Angle n
  , turtleStack :: [TurtleState n]
  }

push :: TurtleState n -> Environment n -> Environment n
push t (Environment a ts) = Environment a (t:ts)

pop :: Environment n -> Environment n
pop (Environment a (t:ts)) = Environment a ts

incAngle :: Num n => n -> Environment n -> Environment n
incAngle n (Environment a ts) = Environment (fmap (* n) a) ts

-- | Successive generations of the production rules applied to the
--   starting symbols.
generations :: Ord n => Rules n -> [Symbol n] -> [[Symbol n]]
generations dict syms = iterate (concatMap (produce dict)) syms
  where
  produce d s = fromMaybe [s] (M.lookup s d)

-- | Interpret a list of symbols as turtle graphics commands
--   to create a 'TurtleState'. The turtle data is tracked in a Reader monad.
lSystemR :: (Floating n, Ord n) => [Symbol n] -> Reader (Environment n) (TurtleState n)
lSystemR syms = go startTurtle syms
  where
  go turtle []     = return turtle
  go turtle (x:xs) = case x of
    F       -> go (forward 1 . penDown $ turtle) xs
    G       -> go (forward 1 . penUp   $ turtle) xs
    Plus    -> do
      env <- ask
      go (left  (angleInc env ^. deg) turtle) xs
    Minus   -> do
      env <- ask
      go (right (angleInc env ^. deg) turtle) xs
    Reverse -> go (left 180 turtle) xs
    Push    -> local (push (penUp turtle)) (go turtle xs)
    Pop     -> do
      env <- ask
      case turtleStack env of
        []    -> error "Nothing to pop"
        (t:_) -> local pop $ go (t { currTrail = currTrail turtle
                                   , paths = paths turtle}) xs
    Width w -> go (setPenWidth ((* (1+w)) <$> (penWidth . currPenStyle $ turtle))
                               turtle) xs
    Delta d -> local (incAngle (1+d)) (go turtle xs)
    _       -> go turtle xs

-- | Create a 'TurtelState' using n iterations of the rules with given axiom symbols
--   and the angle increment, delta. The first segment is in the unitX direction.
lSystem :: (Floating n, Ord n)
         => Int -> Angle n -> [Symbol n] -> Rules n -> TurtleState n
lSystem n delta axiom rules =
  runReader (lSystemR (generations rules axiom !! n)) (Environment delta [])

-- | Create a path using n iterations of the rules with given axiom symbols
--   and the angle increment, delta. The first segment is in the unitX direction.
--   The styles in the 'TurtleState' are ignored.
lSystemPath :: (Floating n, Ord n)
             => Int -> Angle n -> [Symbol n] -> Rules n -> Path V2 n
lSystemPath n delta axiom rules = getTurtlePath $ lSystem n delta axiom rules

-- | Create a diagram using n iterations of the rules with given axiom symbols
--   and the angle increment, delta. The first segment is in the unitX direction.
--   The styles in the 'TurtleState' are applied to the trails in the diagram.
lSystemDiagram :: (TypeableFloat n, Renderable (Path V2 n) b)
                => Int -> Angle n -> [Symbol n] -> Rules n -> QDiagram b V2 n Any
lSystemDiagram n delta axiom rules = getTurtleDiagram $ lSystem n delta axiom rules

symbol :: Fractional n => Char -> Symbol n
symbol 'F' = F
symbol 'G' = G
symbol 'f' = G
symbol '+' = Plus
symbol '-' = Minus
symbol '!' = Reverse
symbol '[' = Push
symbol ']' = Pop
symbol 'X' = X 0
symbol 'Y' = X 1
symbol 'Z' = X 2
symbol 'A' = X 3
symbol 'B' = X 4
symbol 'C' = X 5
symbol '<' = Width 0.1
symbol '>' = Width (-0.1)
symbol '(' = Delta 0.1
symbol ')' = Delta (-0.1)
symbol c   = error ("Invalid character " ++ [c])

symbols :: Fractional n => String -> [Symbol n]
symbols = map symbol

rule :: Fractional n => Char -> String -> (Symbol n, [Symbol n])
rule c cs = (symbol c, symbols cs)

-------------------------------------------------------------------------------
-- Examples

-- > import Diagrams.TwoD.Path.LSystem
-- > sierpinskiEx = lwO 4 . stroke . getTurtlePath $ sierpinski 6 :: Diagram B

-- | Sierpinski triangle.
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_sierpinskiEx.svg#diagram=sierpinskiEx&height=400>>
sierpinski :: RealFloat n => Int -> TurtleState n
sierpinski n = lSystem n (60 @@ deg) (symbols "FX") rules
  where
  rules = M.fromList [ rule 'F' "Z"
                     , rule 'X' "+FY-FX-FY+"
                     , rule 'Y' "-FX+FY+FX-" ]

-- > import Diagrams.TwoD.Path.LSystem
-- > cantorEx = cantor 4 :: Diagram B

-- | Cantor dust
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_cantorEx.svg#diagram=cantorEx&width=400>>
cantor :: (Renderable (Path V2 n) b, TypeableFloat n) => Int -> QDiagram b V2 n Any
cantor n = vsep 0.05 $ map dust [0..n]
  where
  dust i =  scaleToX 1 . lw ultraThick $ lSystemDiagram i (0 @@ turn) (symbols "F") rules
  rules = M.fromList [ rule 'F' "FfF"
                     , rule 'f' "fff" ]

-- > import Diagrams.TwoD.Path.LSystem
-- > dragonEx = rotateBy (1/4) . getTurtleDiagram $ dragon 10 :: Diagram B

-- | Dragon curve
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_dragonEx.svg#diagram=dragonEx&height=400>>
dragon :: RealFloat n => Int -> TurtleState n
dragon n = lSystem n (90 @@ deg) (symbols "FX") rules
  where
  rules = M.fromList [ rule 'F' "Z"
                       , rule 'X' "FX+FY+"
                       , rule 'Y' "-FX-FY" ]

-- > import Diagrams.TwoD.Path.LSystem
-- > hexGosperEx = rotateBy (1/4) . getTurtleDiagram $ hexGosper 4 :: Diagram B

-- | Hexagonal Gosper curve
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_hexGosperEx.svg#diagram=hexGosperEx&height=400>>
hexGosper :: RealFloat n => Int -> TurtleState n
hexGosper n = lSystem n (60 @@ deg) (symbols "FX") hex
  where
  hex = M.fromList [ rule 'F' "Z"
                   , rule 'X' "FX+FY++FY-FX--FXFX-FY+"
                   , rule 'Y' "-FX+FYFY++FY+FX--FX-FY" ]

-- > import Diagrams.TwoD.Path.LSystem
-- > kochIslandEx = lwO 3 . stroke . getTurtlePath $ kochIsland 3 :: Diagram B

-- | Koch Island
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_kochIslandEx.svg#diagram=kochIslandEx&height=400>>
kochIsland :: RealFloat n => Int -> TurtleState n
kochIsland n = lSystem n (90 @@ deg) axiom koch
  where
  koch  = M.fromList [rule 'F' "F-F+F+FF-F-F+F"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > kochLakeEx = rotateBy (1/4) . getTurtleDiagram $ kochLake 2 :: Diagram B

-- | Koch lake
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_kochLakeEx.svg#diagram=kochLakeEx&height=400>>
kochLake :: RealFloat n => Int -> TurtleState n
kochLake n = lSystem n (1/4 @@ turn) (symbols "F+F+F+F") lake
  where
  lake = M.fromList [ rule 'F' "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF"
                    , rule 'f' "ffffff"]

-- > import Diagrams.TwoD.Path.LSystem
-- > koch1Ex = lwO 3 . stroke . getTurtlePath $ koch1 4 :: Diagram B

-- | Koch curve 1
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch1Ex.svg#diagram=koch1Ex&height=400>>
koch1 :: RealFloat n => Int -> TurtleState n
koch1 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  = M.fromList [rule 'F' "FF-F-F-F-F-F+F"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > koch2Ex =  getTurtleDiagram $ koch2 4 :: Diagram B

-- | Koch curve 2
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch2Ex.svg#diagram=koch2Ex&height=400>>
koch2 :: RealFloat n => Int -> TurtleState n
koch2 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  = M.fromList [rule 'F' "FF-F-F-F-FF"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > koch3Ex =  getTurtleDiagram $ koch3 3 :: Diagram B

-- | Koch curve 3
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch3Ex.svg#diagram=koch3Ex&height=400>>
koch3 :: RealFloat n => Int -> TurtleState n
koch3 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  =M.fromList [rule 'F' "FF-F+F-F-FF"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > koch4Ex =  getTurtleDiagram $ koch4 4 :: Diagram B

-- | Koch curve 4
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch4Ex.svg#diagram=koch4Ex&height=400>>
koch4 :: RealFloat n => Int -> TurtleState n
koch4 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  = M.fromList [rule 'F' "FF-F--F-F"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > koch5Ex =  getTurtleDiagram $ koch5 5:: Diagram B

-- | Koch curve 5
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch5Ex.svg#diagram=koch5Ex&height=400>>
koch5 :: RealFloat n => Int -> TurtleState n
koch5 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  = M.fromList [rule 'F' "F-FF--F-F"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > koch6Ex =  getTurtleDiagram $ koch6 4:: Diagram B

-- | Koch curve 6
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_koch6Ex.svg#diagram=koch6Ex&height=400>>
koch6 :: RealFloat n => Int -> TurtleState n
koch6 n = lSystem n (1/4 @@ turn) axiom koch
  where
  koch  = M.fromList [rule 'F' "F-F+F-F-F"]
  axiom = symbols "F-F-F-F"

-- > import Diagrams.TwoD.Path.LSystem
-- > tree1Ex =  rotateBy (1/4) . getTurtleDiagram $ tree1 5 :: Diagram B

-- | Tree 1
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree1Ex.svg#diagram=tree1Ex&width=150>>
tree1 :: RealFloat n => Int -> TurtleState n
tree1 n = lSystem n (1/14 @@ turn) (symbols "F") tree
  where
  tree  = M.fromList [rule 'F' "F[+F]F[-F]F"]

-- > import Diagrams.TwoD.Path.LSystem
-- > tree2Ex =  rotateBy (1/4) . getTurtleDiagram $ tree2 6 :: Diagram B

-- | Tree 2
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree2Ex.svg#diagram=tree2Ex&height=400>>
tree2 :: RealFloat n => Int -> TurtleState n
tree2 n = lSystem n (1/18 @@ turn) (symbols "F") tree
  where
  tree  = M.fromList [rule 'F' "F[+>>>F]F[->>>F][>>>F]"]

-- > import Diagrams.TwoD.Path.LSystem
-- > tree3Ex =  rotateBy (1/4) . getTurtleDiagram $ tree3 4 :: Diagram B

-- | Tree 3
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree3Ex.svg#diagram=tree3Ex&height=400>>
tree3 :: RealFloat n => Int -> TurtleState n
tree3 n = lSystem n (1/16 @@ turn) (symbols "F") tree
  where
  tree  = M.fromList [rule 'F' "FF-[->F+>F+>F]+[+>F->F->F]"]

-- > import Diagrams.TwoD.Path.LSystem
-- > tree4Ex =  rotateBy (1/4) . getTurtleDiagram $ tree4 7 :: Diagram B

-- | Tree 4
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree4Ex.svg#diagram=tree4Ex&height=400>>
tree4 :: RealFloat n => Int -> TurtleState n
tree4 n = lSystem n (1/18 @@ turn) (symbols "X") tree
  where
  tree  = M.fromList [ rule 'X' "F>>[+X]F>>[-X]+X"
                     , rule 'F' "FF"]

-- > import Diagrams.TwoD.Path.LSystem
-- > tree5Ex =  rotateBy (1/4) . getTurtleDiagram $ tree5 7 :: Diagram B

-- | Tree 5
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree5Ex.svg#diagram=tree5Ex&height=400>>
tree5 :: RealFloat n => Int -> TurtleState n
tree5 n = lSystem n (1/14 @@ turn) (symbols "X") tree
  where
  tree = M.fromList [ rule 'X' "F[+>>X][->>X]F>X"
                    , rule 'F' "FF"]

-- > import Diagrams.TwoD.Path.LSystem
-- > tree6Ex =  rotateBy (1/4) . getTurtleDiagram $ tree6 5 :: Diagram B

-- | Tree 6
--
--   <<diagrams/src_Diagrams_TwoD_Path_LSystem_tree6Ex.svg#diagram=tree6Ex&height=400>>
tree6 :: RealFloat n => Int -> TurtleState n
tree6 n = lSystem n (1/16 @@ turn) (symbols "X") tree
  where
  tree = M.fromList [ rule 'X' "F-[[>>X]+X]+F[+F>>X]-X"
                    , rule 'F' "FF"]
