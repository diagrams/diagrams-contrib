{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.Constrained
-- Copyright   :  (c) 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Lay out diagrams by specifying constraints.  Currently, the API is
-- fairly simple: only equational constraints are supported (not
-- inequalities), and you can only use it to compose a collection of
-- diagrams (and not to, say, compute the position of some point).
-- Future versions may support additional features.
--
-- As a basic example, we can introduce a circle and a square, and
-- constrain them to be next to each other:
--
-- > import Diagrams.TwoD.Layout.Constrained
-- >
-- > constrCircleSq = frame 0.2 $ layout $ do
-- >   c <- newDia (circle 1)
-- >   s <- newDia (square 2)
-- >   constrainWith hcat [c, s]
--
-- We start a block of constraints with 'layout'; introduce new
-- diagrams with 'newDia', and then constrain them, in this case using
-- the 'constrainWith' function.  The result looks like this:
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Constrained_constrCircleSq.svg#diagram=constrCircleSq&width=300>>
--
-- Of course this is no different than just writing @circle 1 |||
-- square 2@. The interest comes when we start constraining things in
-- more interesting ways.
--
-- For example, the following code creates a row of differently-sized
-- circles with a bit of space in between them, and then draws a
-- square which is tangent to the last circle and passes through the
-- center of the third.  Manually computing the size (and position) of
-- this square would be tedious.  Instead, the square is declared to
-- be scalable, meaning it may be uniformly scaled to accomodate
-- constraints.  Then a point on the left side of the square is
-- constrained to be equal to the center of the third circle, and a
-- point on the right side of the square is made equal to a point on
-- the edge of the rightmost circle.  This causes the square to be
-- automatically positioned and scaled appropriately.
--
-- > import Diagrams.TwoD.Layout.Constrained
-- >
-- > circleRow = frame 1 $ layout $ do
-- >   cirs <- newDias (map circle [1..5])
-- >   constrainWith (hsep 1) cirs
-- >   rc <- newPointOn (last cirs) (envelopeP unitX)
-- >
-- >   sq <- newScalableDia (square 1)
-- >   ls <- newPointOn sq (envelopeP unit_X)
-- >   rs <- newPointOn sq (envelopeP unitX)
-- >
-- >   ls =.= centerOf (cirs !! 2)
-- >   rs =.= rc
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Constrained_circleRow.svg#diagram=circleRow&width=300>>
--
-- As a final example, the following code draws a vertical stack of
-- circles, along with an accompanying set of squares, such that (1)
-- each square constrained to lie on the same horizontal line as a
-- circle (using @zipWithM_ 'sameY'@), and (2) the squares all lie on
-- a diagonal line (using 'along').
--
-- > import Diagrams.TwoD.Layout.Constrained
-- > import Control.Monad (zipWithM_)
-- >
-- > diagonalLayout = frame 1 $ layout $ do
-- >   cirs <- newDias (map circle [1..5] # fc blue)
-- >   sqs  <- newDias (replicate 5 (square 2) # fc orange)
-- >   constrainWith vcat cirs
-- >   zipWithM_ sameY cirs sqs
-- >   constrainWith hcat [cirs !! 0, sqs !! 0]
-- >   along (direction (1 ^& (-1))) (map centerOf sqs)
--
-- <<diagrams/src_Diagrams_TwoD_Layout_Constrained_diagonalLayout.svg#diagram=diagonalLayout&width=400>>
--
-- Take a look at the implementations of combinators such as 'sameX',
-- 'allSame', 'constrainDir', and 'along' for ideas on implementing
-- your own constraint combinators.
--
-- Ideas for future versions of this module:
--
-- * Introduce z-index constraints.  Right now the diagrams are just
--   drawn in the order that they are introduced.
--
-- * A way to specify default values --- /i.e./ be able to introduce
--   new point or scalar variables with a specified default value
--   (instead of just defaulting to the origin or to 1).
--
-- * Doing something more reasonable than crashing for overconstrained
--   systems.
--
-- I am also open to other suggestions and/or pull requests!
-----------------------------------------------------------------------------

module Diagrams.TwoD.Layout.Constrained
       ( -- * Basic types
         Expr, mkExpr, Constrained, ConstrainedState, DiaID

         -- * Layout
       , layout

         -- * Creating constrainable things

         -- | Diagrams, points, /etc./ which will participate in a
         --   system of constraints must first be explicitly
         --   introduced using one of the functions in this section.
       , newDia, newDias, newScalableDia
       , newPoint, newPointOn
       , newScalar

         -- * Diagram accessors

         -- | Combinators for extracting constrainable attributes of
         --   an introduced diagram.
       , centerOf, xOf, yOf, scaleOf

         -- * Constraints
       , (====), (=.=), (=^=)
       , sameX, sameY
       , allSame
       , constrainWith
       , constrainDir
       , along

       )
       where

import qualified Control.Lens         as L
import qualified Control.Lens.Extras  as L
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Foldable        as F
import           Data.Hashable
import           Data.List            (sortBy)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Ord             (comparing)
import           GHC.Generics

import qualified Math.MFSolve         as MFS

import           Diagrams.Coordinates
import           Diagrams.Prelude

------------------------------------------------------------
-- Variables and expressions
------------------------------------------------------------

-- | An abstract type representing unique IDs for diagrams.  The
--   constructor is not exported, so the only way to obtain a 'DiaID'
--   is by calling 'newDia' or 'newDias'. The phantom type parameter
--   's' ensures that such 'DiaID's can only be used with the
--   constrained system in which they were introduced.
newtype DiaID s = DiaID Int
  deriving (Ord, Eq, Show, Generic)

-- | Variables can track one of four things: an x-coordinate, a
--   y-coordinate, a scaling factor, or a length.
data VarType = S   -- ^ scaling factor
             | L   -- ^ length
             | X   -- ^ X-coordinate of a point
             | Y   -- ^ Y-coordinate of a point
  deriving (Eq, Ord, Read, Show, Generic)

-- | A variable has a name and a type, and may optionally be
--   associated to some particular diagram.
data Var s = Var { _varID :: Maybe (DiaID s), _varName :: String, _varType :: VarType }
  deriving (Eq, Ord, Generic, Show)

makeLensesWith (lensRulesFor [("_varType", "varType")]) ''Var

-- Auto-derive Hashable instances using Generic
instance Hashable (DiaID s)
instance Hashable VarType
instance Hashable (Var s)

-- | The type of reified expressions over @Vars@, with
--   numeric values taken from the type @n@.  The important point to
--   note is that 'Expr' is an instance of 'Num', 'Fractional', and
--   'Floating', so 'Expr' values can be combined and manipulated as
--   if they were numeric expressions, even when they occur inside
--   other types.  For example, 2D vector values of type @V2 (Expr s
--   n)@ and point values of type @P2 (Expr s n)@ can be combined
--   using operators such as '.+^', '.-.', and so on, in order to
--   express constraints on vectors and points.
--
--   To create literal 'Expr' values, you can use 'mkExpr'.
--   Otherwise, they are introduced by creation functions such as
--   'newPoint', 'newScalar', or diagram accessor functions like
--   'centerOf' or 'xOf'.
type Expr s n = MFS.Expr (Var s) n

-- | Convert a literal numeric value into an 'Expr'.  To convert
--   structured types such as vectors or points, you can use e.g. @fmap
--   mkExpr :: V2 n -> V2 (Expr s n)@.
mkExpr :: n -> Expr s n
mkExpr = MFS.makeConstant

------------------------------------------------------------
-- Functions for variable and expression creation
------------------------------------------------------------

-- | Create an internal variable corresponding to a diagram, with
--   the given name and variable type.  Not intended to be called by
--   end users.
diaVar :: DiaID s -> String -> VarType -> Var s
diaVar = Var . Just

-- | Create an internal variable unattached to any particular diagram, with
--   a given name and variable type. Not intended to be called by end
--   users.
newVar :: String -> VarType -> Var s
newVar = Var Nothing

-- | Create a variable corresponding to a particular diagram, with a
--   given name and type.  Not intended to be called by end users.
mkDVar :: Num n => DiaID s -> String -> VarType -> Expr s n
mkDVar d s ty = MFS.makeVariable (diaVar d s ty)

-- | Create a variable unattached to any particular diagram, with a
--   a given name and type. Not intended to be called by end users.
mkVar :: Num n => String -> VarType -> Expr s n
mkVar s ty = MFS.makeVariable (newVar s ty)

-- | Make a variable tracking the local origin of a given diagram.
--   Not intended to be called by end users.
mkDPVar :: Num n => DiaID s -> String -> P2 (Expr s n)
mkDPVar d s = mkDVar d s X ^& mkDVar d s Y

-- | Make a variable corresponding to a 2D point.  Not intended to be
--   called by end users.
mkPVar :: Num n => String -> P2 (Expr s n)
mkPVar s = mkVar s X ^& mkVar s Y

------------------------------------------------------------
-- Constraints
------------------------------------------------------------

-- | A set of 'Constraints' is a monadic computation
--   in the 'MFS.MFSolver' monad.  Users need not concern themselves
--   with the details of 'MFS.MFSolver'; it should suffice to think of
--   'Constraints' as an abstract type.
--
--   This type is INTERNAL to the library and should not be exported.
--   There is no need to have two separate layers of combining
--   things---combining Constraints and then also combining
--   Constrained systems, both using a monadic interface.  In the
--   user-facing API, we just immediately turn each Constraints value
--   into a Constrained computation, which can then be combined.
type Constraints s n = MFS.MFSolver (Var s) n ()

-- | The state maintained by the Constrained monad.  Note that @s@
--   is a phantom parameter, used in a similar fashion to the @ST@
--   monad, to ensure that generated diagram IDs do not leak.
data ConstrainedState s b n m = ConstrainedState
  { _equations  :: Constraints s n
                   -- ^ Current set of constraints
  , _diaCounter :: Int
                   -- ^ Global counter for unique diagram IDs
  , _varCounter :: Int
                   -- ^ Global counter for unique variable IDs
  , _diagrams   :: M.Map (DiaID s) (QDiagram b V2 n m)
                   -- ^ Map from diagram IDs to diagrams
  }

makeLenses ''ConstrainedState

-- | The initial ConstrainedState: no equations, no diagrams, and
--   counters at 0.
initConstrainedState :: ConstrainedState s b n m
initConstrainedState = ConstrainedState
  { _equations  = return ()
  , _diaCounter = 0
  , _varCounter = 0
  , _diagrams   = M.empty
  }

-- | A monad for constrained systems.  It suffices to think of it as
--   an abstract monadic type; the constructor for the internal state
--   is intentionally not exported.  'Constrained' values can be
--   created using the combinators below; combined using the @Monad@
--   interface; and discharged by the 'layout' function.
--
--   Note that @s@ is a phantom parameter, used in a similar fashion
--   to the 'ST' monad, to ensure that generated diagram IDs cannot be
--   mixed between different 'layout' blocks.
type Constrained s b n m a = State (ConstrainedState s b n m) a

------------------------------------------------------------
-- Constraint DSL
------------------------------------------------------------

--------------------------------------------------
-- Creating constrainable things

-- | Introduce a new diagram into the constrained system.  Returns a
--   unique ID for use in referring to the diagram later.
--
--   The position of the diagram's origin may be constrained.  If
--   unconstrained, the origin will default to (0,0).  For a diagram
--   whose scaling factor may also be constrained, see
--   'newScalableDia'.
newDia
  :: (Hashable n, Floating n, RealFrac n)
  => QDiagram b V2 n m -> Constrained s b n m (DiaID s)
newDia dia = do
  d <- newScalableDia dia
  scaleOf d ==== 1
  return d

-- | Introduce a new diagram into the constrained system.  Returns a
--   unique ID for use in referring to the diagram later.
--
--   Both the position of the diagram's origin and its scaling factor
--   may be constrained.  If unconstrained, the origin will default to
--   (0,0), and the scaling factor to 1, respectively.
newScalableDia :: QDiagram b V2 n m -> Constrained s b n m (DiaID s)
newScalableDia dia = do
  d <- DiaID <$> (diaCounter <+= 1)
  diagrams . L.at d ?= dia
  return d

-- | Introduce a list of diagrams into the constrained system.
--   Returns a corresponding list of unique IDs for use in referring
--   to the diagrams later.
newDias
  :: (Hashable n, Floating n, RealFrac n)
  => [QDiagram b V2 n m] -> Constrained s b n m [DiaID s]
newDias = mapM newDia

--------------------------------------------------
-- Constrained points etc.

-- | The point at the center (i.e. local origin) of the given
--   diagram.  For example, to constrain the origin of diagram @b@ to
--   be offset from the origin of diagram @a@ by one unit to the right
--   and one unit up, one may write
--
--   @centerOf b =.= centerOf a .+^ (1 ^& 1)@
centerOf :: Num n => DiaID s -> P2 (Expr s n)
centerOf d = mkDPVar d "center"

-- | The x-coordinate of the center for the given diagram, which can
--   be used in constraints to determine the x-position of this
--   diagram relative to others.
--
--   For example,
--
--   @xOf d1 + 2 === xOf d2@
--
--   constrains diagram @d2@ to lie 2 units to the right of @d1@ in
--   the horizontal direction, though it does not constrain their
--   relative positioning in the vertical direction.
xOf :: Num n => DiaID s -> Expr s n
xOf d = mkDVar d "center" X

-- | The y-coordinate of the center for the given diagram, which can
--   be used in constraints to determine the y-position of this
--   diagram relative to others.
--
--   For example,
--
--   @allSame (map yOf ds)@
--
--   constrains the diagrams @ds@ to all lie on the same horizontal
--   line.
yOf :: Num n => DiaID s -> Expr s n
yOf d = mkDVar d "center" Y

-- | The scaling factor applied to this diagram.
--
--   For example,
--
--   @scaleOf d1 === 2 * scaleOf d2@
--
--   constrains @d1@ to be scaled twice as much as @d2@. (It does not,
--   however, guarantee anything about their actual relative sizes;
--   that depends on their relative size when unscaled.)
--
scaleOf :: Num n => DiaID s -> Expr s n
scaleOf d = mkDVar d "scale" S

-- | Create a new (constrainable) point attached to the given diagram,
--   using a function that picks a point given a diagram.
--
--   For example, to get the point on the right edge of a diagram's
--   envelope, one may write
--
--   @rt <- newPointOn d (envelopeP unitX)@
--
--   To get the point (1,1),
--
--   @one_one <- newPointOn d (const (1 ^& 1))@
--
--   This latter example is far from useless: note that @one_one@ now
--   corresponds not to the absolute coordinates (1,1), but to the
--   point which lies at (1,1) /relative to the unscaled diagram's
--   origin/.  If the diagram is positioned or scaled to satisfy some
--   other constraints, @one_one@ will move right along with it.
--
--   For example, the following code establishes a small circle which
--   is located at a specific point relative to a big circle.  The
--   small circle is carried along with the big circle as it is laid
--   out in between some squares.
--
--   > import Diagrams.TwoD.Layout.Constrained
--   >
--   > circleWithCircle = frame 0.3 $ layout $ do
--   >   c2 <- newScalableDia (circle 2)
--   >   p <- newPointOn c2 (const $ (1 ^& 0) # rotateBy (1/8))
--   >
--   >   c1 <- newDia (circle 1)
--   >   centerOf c1 =.= p
--   >
--   >   [a,b] <- newDias (replicate 2 (square 2))
--   >   constrainWith hcat [a,c2,b]
--
--   <<diagrams/src_Diagrams_TwoD_Layout_Constrained_circleWithCircle.svg#diagram=circleWithCircle&width=300>>

newPointOn
  :: (Hashable n, Floating n, RealFrac n)
  => DiaID s
  -> (QDiagram b V2 n m -> P2 n)
  -> Constrained s b n m (P2 (Expr s n))
newPointOn d getP = do
  -- the fromJust is justified, because the type discipline on DiaIDs ensures
  -- they will always represent a valid index in the Map.
  dia <- fromJust <$> use (diagrams . L.at d)
  let p = getP dia

  v <- varCounter <+= 1
  let newPt = mkDPVar d ("a" ++ show v)

  -- constrain the new point to move relative to the diagram origin,
  -- taking possible scaling into account
  centerOf d .+^ (scaleOf d *^ (mkExpr <$> (p .-. origin))) =.= newPt

  return newPt

-- | Introduce a new constrainable point, unattached to any particular
--   diagram.  If either of the coordinates are still unconstrained at
--   the end, they will default to zero.
newPoint :: Num n => Constrained s b n m (P2 (Expr s n))
newPoint = do
  v <- varCounter <+= 1
  return $ mkPVar ("a" ++ show v)

-- | Introduce a new scalar value which can be constrained.  If still
--   unconstrained at the end, it will default to 1.
newScalar :: Num n => Constrained s b n m (Expr s n)
newScalar = do
  v <- varCounter <+= 1
  return $ mkVar ("s" ++ show v) S

--------------------------------------------------
-- Specifying constraints

-- | Apply some constraints.
constrain :: Constraints s n -> Constrained s b n m ()
constrain newConstraints = equations %= (>> newConstraints)
  -- XXX should this be right-nested instead?  Does it matter?

infix 1 =.=, =^=, ====

-- | Constrain two scalar expressions to be equal.
--   Note that you need not worry about introducing redundant
--   constraints; they are ignored.
(====)
  :: (Floating n, RealFrac n, Hashable n)
  => Expr s n -> Expr s n -> Constrained s b n m ()
a ==== b = constrain $ MFS.ignore (a MFS.=== b)

-- | Constrain two points to be equal.
(=.=)
  :: (Hashable n, Floating n, RealFrac n)
  => P2 (Expr s n) -> P2 (Expr s n) -> Constrained s b n m ()
(coords -> px :& py) =.= (coords -> qx :& qy) = do
  px ==== qx
  py ==== qy

-- | Constrain two vectors to be equal.
(=^=)
  :: (Hashable n, Floating n, RealFrac n)
  => V2 (Expr s n) -> V2 (Expr s n) -> Constrained s b n m ()
(coords -> px :& py) =^= (coords -> qx :& qy) = do
  px ==== qx
  py ==== qy

-- | Constrain a collection of diagrams to be positioned relative to
--   one another according to a function such as 'hcat', 'vcat', 'hsep',
--   and so on.
--
--   A typical use would be
--
-- @
-- cirs <- newDias (map circle [1..5])
-- constrainWith (hsep 1) cirs
-- @
--
--   which creates five circles and constrains them to be positioned
--   in a row, with one unit of space in between adjacent pairs.
--
--   The funny type signature is something of a hack.  The sorts of
--   functions which should be passed as the first argument to
--   'constrainWith' tend to be highly polymorphic; 'constrainWith'
--   uses a concrete type which it can use to extract relevant
--   information about the function by observing its behavior.  In
--   short, you do not need to know anything about @Located Envelope@s
--   in order to call this function.
constrainWith
  :: (Hashable n, RealFrac n, Floating n, Monoid' m)
  => -- (forall a. (...) => [a] -> a)
     ([[Located (Envelope V2 n)]] -> [Located (Envelope V2 n)])
  -> [DiaID s]
  -> Constrained s b n m ()
constrainWith _ [] = return ()
constrainWith f hs = do
  diaMap <- use diagrams
  let dias  = map (fromJust . flip M.lookup diaMap) hs
      envs  = map ((:[]) . (`at` origin) . getEnvelope) dias
      envs' = f envs
      eCtrs = map loc envs'
      offs  = zipWith (.-.) (tail eCtrs) eCtrs
      rtps  = zipWith envelopeP             offs (init envs')
      ltps  = zipWith (envelopeP . negated) offs (tail envs')
      gaps'  = (map . fmap) mkExpr $ zipWith (.-.) ltps rtps
  rts <- zipWithM newPointOn (init hs) (map envelopeP offs)
  lts <- zipWithM newPointOn (tail hs) (map (envelopeP . negated) offs)
  zipWithM3_ (\r g l -> r .+^ g =.= l) rts gaps' lts

zipWithM3_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWithM3_ f as bs cs = sequence_ $ zipWith3 f as bs cs

-- | Constrain the origins of two diagrams to have the same
--   x-coordinate.
sameX
  :: (Hashable n, Floating n, RealFrac n)
  => DiaID s -> DiaID s -> Constrained s b n m ()
sameX h1 h2 = xOf h1 ==== xOf h2

-- | Constrain the origins of two diagrams to have the same
--   y-coordinate.
sameY
  :: (Hashable n, Floating n, RealFrac n)
  => DiaID s -> DiaID s -> Constrained s b n m ()
sameY h1 h2 = yOf h1 ==== yOf h2

-- | Constrain a list of scalar expressions to be all equal.
allSame
  :: (Hashable n, Floating n, RealFrac n)
  => [Expr s n] -> Constrained s b n m ()
allSame as = zipWithM_ (====) as (tail as)

-- | @constrainDir d p q@ constrains the direction from @p@ to @q@ to
--   be @d@.  That is, the direction of the vector @q .-. p@ must be
--   @d@.
constrainDir :: (Hashable n, Floating n, RealFrac n) => Direction V2 (Expr s n) -> P2 (Expr s n) -> P2 (Expr s n) -> Constrained s b n m ()
constrainDir dir p q = do
  s <- newScalar
  p .+^ (s *^ fromDirection dir) =.= q

-- | @along d ps@ constrains the points @ps@ to all lie along a ray
--   parallel to the direction @d@.
along :: (Hashable n, Floating n, RealFrac n) => Direction V2 (Expr s n) -> [P2 (Expr s n)] -> Constrained s b n m ()
along dir ps = zipWithM_ (constrainDir dir) ps (tail ps)

------------------------------------------------------------
-- Constraint resolution
------------------------------------------------------------

-- | A data type holding a variable together with its resolution
--   status: its solved value, if it exists, or Nothing if the
--   variable is still unresolved.
data VarResolution s n = VR { _resolvedVar :: Var s, _resolution :: Maybe n }

makeLenses ''VarResolution

-- | Check whether a variable has been resolved.
isResolved :: VarResolution s n -> Bool
isResolved = L.is _Just . view resolution

-- | Get the three variables associated with a diagram: X, Y, and
--   Scale.
getDiaVars
  :: MFS.Dependencies (Var s) n -> DiaID s -> M.Map VarType (VarResolution s n)
getDiaVars deps d = M.fromList $
  [ (X, getRes (diaVar d "center" X))
  , (Y, getRes (diaVar d "center" Y))
  , (S, getRes (diaVar d "scale"  S))
  ]
  where
    getRes v
      = VR v (either (const Nothing) Just $ MFS.getKnown v deps)

-- | Solve a constrained system, combining the resulting diagrams with
--   'mconcat'.  This is the top-level function for introducing a
--   constrained system, and is the only way to generate an actual
--   diagram.
--
--   Redundant constraints are ignored.  If there are any
--   unconstrained diagram variables remaining, they are given default
--   values one at a time, beginning with defaulting remaining scaling
--   factors to 1, then defaulting x- and y-coordinates to zero.
--
--   An overconstrained system will cause 'layout' to simply crash.
--   This is obviously not ideal.  A future version may do something
--   more reasonable.
layout
  :: (Monoid' m, Hashable n, Floating n, RealFrac n, Show n)
  => (forall s. Constrained s b n m a)
  -> QDiagram b V2 n m
layout constr =
  case MFS.execSolver (MFS.ignore $ s ^. equations) MFS.noDeps of
    Left _depError -> error "overconstrained"
    Right deps    ->
      let deps' = resolve (map fst dias) deps
      in  mconcat . flip map dias $ \(d, dia) ->
        let vars = getDiaVars deps' d
            expectedRes ty = vars ^?! L.at ty . _Just . resolution . _Just
        in
          case F.all (isResolved) vars of
            True -> dia # scale (expectedRes S)
                        # translate (expectedRes X ^& expectedRes Y)
            _ -> error . unlines $
                 [ "Diagrams.TwoD.Layout.Constrained.layout: impossible!"
                 , "Diagram variables not resolved. Please report this as a bug:"
                 , "  https://github.com/diagrams/diagrams-contrib/issues"
                 ]
                 -- 'resolve' should always set the S, X, and Y variables for
                 -- a diagram if they aren't already constrained, so getDiaVars
                 -- should return three resolved variables
  where
    s = execState constr initConstrainedState
    dias = M.assocs (s ^. diagrams)

resolve
  :: (Hashable n, RealFrac n, Floating n, Show n)
  => [DiaID s] -> MFS.Dependencies (Var s) n -> MFS.Dependencies (Var s) n
resolve diaIDs deps =
  case unresolved of
    [] -> deps
    ((VR v _) : _) ->
      let eq = MFS.makeVariable v - (if v^.varType == S then 1 else 0)
      in case MFS.addEquation deps eq of
               Right deps' -> resolve diaIDs deps'
               Left err    -> error . unlines $
                 [ "Diagrams.TwoD.Layout.Constrained.layout: impossible!"
                 , "Adding equation for unconstrained variable produced an error:"
                 , show err
                 , "Please report this as a bug:"
                 , "  https://github.com/diagrams/diagrams-contrib/issues"
                 ]
  where
    diaVars = diaIDs >>= (M.elems . getDiaVars deps)
    unresolved
      = sortBy (comparing (view (resolvedVar.varType)))
      . filter (not . isResolved)
      $ diaVars
