{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot
  ( HasField (..),
    RecordDotOptics (..),
    GenericDotOptics (..),
    the,
  )
where

import Data.Coerce
import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( RecordDotOptics name v a b u,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % dotOptic @name @v @a @b @u

-- type role RecordDotOptics nominal nominal nominal nominal nominal nominal
class RecordDotOptics name t a b s | name s -> t a b, name t -> s a b where
  dotOptic :: Lens s t a b

type GenericDotOptics :: Symbol -> Type -> Type -> Type -> Type -> Type
newtype GenericDotOptics name t a b s = GenericDotOptics s

instance (GField name s t a b) => RecordDotOptics name t a b (GenericDotOptics name t a b s) where
  dotOptic = coerceS (gfield @name)

the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
