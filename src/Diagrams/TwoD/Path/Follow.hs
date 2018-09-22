{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path.Follow
-- Copyright   :  (c) 2016 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- An alternative monoid for trails which rotates trails so their
-- starting and ending tangents match at join points.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path.Follow where
    -- ( Following, follow, unfollow
    -- ) where

-- import           Diagrams.Prelude

-- import           Data.Monoid.SemiDirectProduct.Strict

-- -- | @Following@ is just like @Trail' Line V2@, except that it has a
-- --   different 'Monoid' instance.  @Following@ values are
-- --   concatenated, just like regular lines, except that they are also
-- --   rotated so the tangents match at the join point.  In addition,
-- --   they are normalized so the tangent at the start point is in the
-- --   direction of the positive x axis (essentially we are considering
-- --   trails equivalent up to rotation).
-- --
-- --   Pro tip: you can concatenate a list of trails so their tangents
-- --   match using 'ala' from "Control.Lens", like so:
-- --
-- --     @ala follow foldMap :: [Trail' Line V2 n] -> Trail' Line V2 n@
-- --
-- --   This is illustrated in the example below.
-- --
-- --   <<#dia=followExample&width=400>>
-- --
-- --   > import Control.Lens (ala)
-- --   >
-- --   > wibble :: Trail' Line V2 Double
-- --   > wibble = hrule 1 <> hrule 0.5 # rotateBy (1/6) <> hrule 0.5 # rotateBy (-1/6) <> a
-- --   >   where a = arc (xDir # rotateBy (-1/4)) (1/5 @@ turn)
-- --   >           # scale 0.7
-- --   >
-- --   > followExample =
-- --   >   [ wibble
-- --   >   , wibble
-- --   >     # replicate 5
-- --   >     # ala follow foldMap
-- --   >   ]
-- --   >   # map stroke
-- --   >   # map centerXY
-- --   >   # vsep 1
-- --   >   # frame 0.5
-- --
-- newtype Following n
  -- = Following { unFollowing :: Semi (Trail' Line V2 n) (Angle n) }
  -- deriving (Monoid)

-- -- | Note this is only an iso when considering trails equivalent up to
-- --   rotation.
-- instance RealFloat n => Wrapped (Following n) where
  -- type Unwrapped (Following n) = Trail' Line V2 n

  -- _Wrapped' = iso unfollow follow

-- instance RealFloat n => Rewrapped (Following n) (Following n')

-- -- | Create a @Following@ from a line, normalizing it (by rotation)
-- --   so that it starts in the positive x direction.
-- follow :: RealFloat n => Trail' Line V2 n -> Following n
-- follow t = Following $ (t # rotate (signedAngleBetween unitX s)) `tag` theta
  -- where
    -- s     = tangentAtStart t
    -- e     = tangentAtEnd t
    -- theta = signedAngleBetween e s

-- -- | Project out the line from a `Following`.
-- --
-- --   If trails are considered equivalent up to rotation, then
-- --   'unfollow' and 'follow' are inverse.
-- unfollow :: Following n -> Trail' Line V2 n
-- unfollow = untag . unFollowing
