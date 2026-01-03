{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot where

import Data.Kind
import GHC.Records
import Optics.Core
import Data.Coerce

instance
  ( RecordDotOptics name u v a b u,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % dotOptic @name @u @v @a @b @u

class RecordDotOptics name s t a b ß | name s -> t a b, name t -> s a b where
-- class RecordDotOptics name s t a b ß | name s -> t a b, name t -> s a b where
  dotOptic :: Lens s t a b

newtype GenericDotOptics r = GenericDotOptics r

instance  (GField name s t a b) => RecordDotOptics name s t a b (GenericDotOptics ß) where
  dotOptic = coerceS (gfield @name)

the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
