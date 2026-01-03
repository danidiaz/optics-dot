{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Lens.Dot where

import Data.Kind
import GHC.Records
import Optics.Core

instance
  ( RecordDotOptics name u v a b,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % dotOptic @name

the :: Iso a b a b
the = Optics.Core.equality

class RecordDotOptics name u v a b | name u -> v a b, name v -> u a b where
  dotOptic :: Lens u v a b

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
