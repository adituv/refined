{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Provides type-level signed integers using existing machinery for
--   type-level natural numbers
module Data.TypeInt(
  -- * Type-level Naturals
  Nat, KnownNat, natVal,

  -- * Type-level Integers
  KnownInt(..), Neg,

  -- * Comparison Operators
  type (<=?), type (<), type (<=), type (>), type (>=)
  ) where

import Data.Proxy(Proxy(..))
import Data.Typeable(Typeable)
import GHC.TypeLits(Nat, KnownNat, natVal)
import qualified GHC.TypeLits as G

-- | Represents a negative number
data Neg (n :: Nat)
  deriving Typeable

-- | Analogue to 'KnownNat', gives the integer associated with a type-level
--   integer.
class KnownInt (n :: k) where
    intVal :: proxy n -> Integer

instance {-# OVERLAPPABLE #-} forall n. KnownNat n => KnownInt n where
    intVal = natVal

instance {-# OVERLAPPING #-} forall n. KnownNat n => KnownInt (Neg n) where
    intVal _ = negate $ natVal (Proxy @n)

type family (x :: k1) <=? (y :: k2) :: Bool where
    Neg x <=? Neg y = y <=? x
    Neg x <=? y     = 'True
    0     <=? Neg 0 = 'True
    x     <=? y     = (G.<=?) x y

type x <  y = (y <=? x) ~ 'False
type x <= y = (x <=? y) ~ 'True
type x >  y = (x <=? y) ~ 'False
type x >= y = (y <=? x) ~ 'True
