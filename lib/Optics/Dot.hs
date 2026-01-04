{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Optics.Dot where

import Data.Kind
import GHC.Records
import Optics.Core
import GHC.TypeLits
import Data.Proxy

instance
  ( 
    HasOpticsMethod u,
    RecordDotOptics (Method u) name u v a b,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % (dotOptic @(Method u) @name @u @v @a @b)


class HasOpticsMethod s where
  type Method s :: Type

-- | 
-- The @name v -> u a b w@ fundep could be added but doesn't seem to be necessary.
-- Could it improve inference?
type RecordDotOptics :: Type -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class RecordDotOptics method name u v a b | name u -> a b, name v -> u a b where
  dotOptic :: Lens u v a b

data GenericsDotOptics

instance (HasOpticsMethod s, 
          Method s ~ GenericsDotOptics, 
          GField name s t a b) => 
          RecordDotOptics GenericsDotOptics name s t a b where
  dotOptic = gfield @name


data FieldDotOptics

instance
  ( 
    HasOpticsMethod a,
    Method a ~ FieldDotOptics,
    HasField name a b,
    SetField name a b
  ) =>
  RecordDotOptics FieldDotOptics name a a b b
  where
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))


the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
