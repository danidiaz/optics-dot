{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An orphan 'HasField' instance (along with some supporting machinery) for
-- the 'Optics.Core.Optic' datatype, that lets you use dot-access syntax on an
-- 'Optic', resulting in a new 'Optic' that \"zooms in\" further into some
-- field.
--
-- Here are some example records. Note how 'DotOptics' is derived via
-- 'GenericFields'.
--
--
-- >>> :{
-- data Whole a = Whole
--   { whole1 :: Int,
--     part :: Part a
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericFields (Whole a)
-- --
-- data Part a = Part
--   { part1 :: Bool,
--     subpart :: Subpart a
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericFields (Part a)
-- --
-- data Subpart a = Subpart
--   { wee :: String,
--     foo :: a,
--     yet :: YetAnotherSubpart
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericFields (Subpart a)
-- --
-- data YetAnotherSubpart = YetAnotherSubpart
--   { ooo :: String,
--     uuu :: Int
--   }
--   deriving (Generic, Show)
--   deriving (DotOptics) via GenericFields YetAnotherSubpart
-- --
-- whole :: Whole Int
-- whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))
-- --
-- nonLensyDotAccess :: String
-- nonLensyDotAccess = whole.part.subpart.yet.ooo
-- :}
--
-- The access chains must start with 'the':
--
-- >>> :{
-- nonTypChanging1 :: Whole Int
-- nonTypChanging1 = whole & the.part.subpart.yet.ooo .~ "newval"
-- :}
--
-- Type-changing updates are supported:
--
-- >>> :{
-- typChanging1 :: Whole Bool
-- typChanging1 = whole & the.part .~ Part True (Subpart "wee" False (YetAnotherSubpart "oldval" 3))
-- --
-- typChanging2 :: Whole Bool
-- typChanging2 = whole & the.part.subpart .~ Subpart "wee" False (YetAnotherSubpart "oldval" 3)
-- --
-- typeChanging3 :: Whole String
-- typeChanging3 = whole & the.part.subpart .~ Subpart "wee" "stuff" (YetAnotherSubpart "oldval" 3)
-- :}
--
-- We can refer to constructs of sum types. Note how 'DotOptics' is derived via 'GenericConstructors':
--
-- >>> :{
-- data Animal a
--   = Dog {name :: String, age :: Int}
--   | Cat {name :: String, purrs :: Bool}
--   | Squirrel { twees :: Bool}
--   | Octopus {tentacles :: Whole a}
--   deriving (Show, Generic)
--   deriving (DotOptics) via GenericConstructors (Animal a)
-- -- 
-- dog :: Animal Int
-- dog = Dog {name = "Fido", age = 5}
-- :}
--
-- The constructor name must be prefixed with an underscore:
--
-- >>> :{
-- matchesDog :: Maybe ([Char], Int)
-- matchesDog = dog ^? the._Dog
-- --
-- matchesSquirrel :: Maybe Bool
-- matchesSquirrel = dog ^? the._Squirrel
-- -- Type-changing update into a branch:
-- changesOctopus :: Animal Bool
-- changesOctopus = dog & the._Octopus.part.subpart.foo .~ False
-- :}
--
--
module Optics.Dot
  ( the,
    DotOptics (..),
    HasDotOptic (..),
    GenericFields (..),
    GenericAffineFields (..),
    GenericConstructors (..),
  )
where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( DotOptics u,
    method ~ DotOpticsMethod u,
    HasDotOptic method name dotName u v a b,
    l ~ DotOpticKind method name u,
    JoinKinds k l m,
    AppendIndices is NoIx ks
  ) =>
  HasField dotName (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % (dotOptic @(DotOpticsMethod u) @name @dotName @u @v @a @b)

-- | Helper typeclass, used to specify the method for deriving dot optics.
-- Usually derived with @DerivingVia@.
--
-- See 'GenericFields', 'GenericAffineFields' and 'GenericConstructors'.
class DotOptics s where
  type DotOpticsMethod s :: Type

-- | Produce an optic according to the given method.
--
type HasDotOptic :: Type -> Symbol -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class
  HasDotOptic method name dotName u v a b
    | -- Usually the name used with dot notation doesn't change, except for constructors.
      name u -> dotName,
      -- Necessary to satisfy the 'HasField' instance.
      dotName u -> name,
      name u -> v a b,
      name v -> u a b
  where
  type DotOpticKind method name u :: OpticKind
  dotOptic :: Optic (DotOpticKind method name u) NoIx u v a b

data GenericFieldsMethod

-- | For deriving 'DotOptics' using @DerivingVia@. The wrapped type is not used for anything.
--
-- Supports type-changing updates.
newtype GenericFields s = MakeGenericFields s

instance DotOptics (GenericFields s) where
  type DotOpticsMethod (GenericFields s) = GenericFieldsMethod

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GField name s t a b,
    name ~ dotName
  ) =>
  HasDotOptic GenericFieldsMethod name dotName s t a b
  where
  type DotOpticKind GenericFieldsMethod name s = A_Lens
  dotOptic = gfield @name

data GenericAffineFieldsMethod

-- | For deriving 'DotOptics' using @DerivingVia@. The wrapped type is not used for anything.
--
-- This is for named fields that may be missing in some branch.
--
-- Supports type-changing updates.
newtype GenericAffineFields s = MakeGenericAffineFields s

instance DotOptics (GenericAffineFields s) where
  type DotOpticsMethod (GenericAffineFields s) = GenericAffineFieldsMethod

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GAffineField name s t a b,
    name ~ dotName
  ) =>
  HasDotOptic GenericAffineFieldsMethod name dotName s t a b
  where
  type DotOpticKind GenericAffineFieldsMethod name s = An_AffineTraversal
  dotOptic = gafield @name

data GenericConstructorsMethod

-- | For deriving 'DotOptics' using @DerivingVia@. The wrapped type is not used for anything.
--
-- Supports type-changing updates.
newtype GenericConstructors s = MakeGenericConstructors s

instance DotOptics (GenericConstructors s) where
  type DotOpticsMethod (GenericConstructors s) = GenericConstructorsMethod

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GConstructor name s t a b,
    -- Dot notation doesn't allow starting with uppercase like constructors do, so we prepend an underscore.
    dotName ~ ConsSymbol '_' name
  ) =>
  HasDotOptic GenericConstructorsMethod name dotName s t a b
  where
  type DotOpticKind GenericConstructorsMethod name s = A_Prism
  dotOptic = gconstructor @name

-- | Identity 'Iso'. Used as a starting point for dot access. A renamed 'Optics.Core.equality'.
the :: Iso s t s t
the = Optics.Core.equality

-- $setup
-- >>> :set -XDerivingVia
-- >>> :set -XDuplicateRecordFields
-- >>> :set -XOverloadedRecordDot
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :set -XNoFieldSelectors
-- >>> :set -XDataKinds
-- >>> import GHC.Generics
-- >>> import Optics.Core
-- >>> import Optics.Dot
