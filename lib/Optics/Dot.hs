{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( DotOptics u,
    HasDotOptic (DotOpticsMethod u) name u v a b,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % (dotOptic @(DotOpticsMethod u) @name @u @v @a @b)

class DotOptics s where
  type DotOpticsMethod s :: Type

-- |
-- The @name v -> u a b w@ fundep could be added but doesn't seem to be necessary.
-- Could it improve inference?
type HasDotOptic :: Type -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class HasDotOptic method name u v a b | name u -> a b where
  dotOptic :: Lens u v a b

data GenericsDotOpticsMethod

newtype GenericsDotOptics s = GenericsDotOptics s

instance DotOptics (GenericsDotOptics s) where
  type DotOpticsMethod (GenericsDotOptics s) = GenericsDotOpticsMethod

instance
  (GField name s t a b) =>
  HasDotOptic GenericsDotOpticsMethod name s t a b
  where
  dotOptic = gfield @name

data FieldDotOpticsMethod

newtype FieldDotOptics s = FieldDotOptics s

instance DotOptics (FieldDotOptics s) where
  type DotOpticsMethod (FieldDotOptics s) = FieldDotOpticsMethod

instance
  ( HasField name s a,
    SetField name s a,
    s ~ t,
    a ~ b
  ) =>
  -- if you change to @name s s a a@, a compilation error happens.
  HasDotOptic FieldDotOpticsMethod name s t a b
  where
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))

the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
