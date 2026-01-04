{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

-- | An orphan 'HasField' instance (along with some supporting machinery) for
-- the 'Optics.Core.Optic' datatype, that lets you use dot-access syntax on an
-- 'Optic', resulting in a new 'Optic' that \"zooms in\" further into some
-- field.
--
-- Here are some example records. Notice how 'DotOptics' is derived with
-- @DerivingVia@, sometimes using 'GenericDotOptics', sometimes using
-- 'FieldDotOptics':
--
-- >>> :{
-- data Whole a = Whole
--   { whole1 :: Int,
--     part :: Part a
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericDotOptics (Whole a)
-- -- 
-- data Part a = Part
--   { part1 :: Bool,
--     subpart :: Subpart a
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericDotOptics (Part a)
-- --
-- data Subpart a = Subpart
--   { wee :: String,
--     foo :: a,
--     yet :: YetAnotherSubpart
--   }
--   deriving stock (Generic, Show)
--   deriving (DotOptics) via GenericDotOptics (Subpart a)
-- --
-- data YetAnotherSubpart = YetAnotherSubpart
--   { ooo :: String,
--     uuu :: Int
--   }
--   deriving (Show)
--   deriving (DotOptics) via FieldDotOptics YetAnotherSubpart
-- --
-- instance SetField "ooo" YetAnotherSubpart String where
--   setField ooo r = r {ooo}
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
-- Type-changing updates are supported when 'DotOptics' is derived via 'GenericDotOptics':
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
module Optics.Dot
  ( 
    the,
    DotOptics (..),
    HasDotOptic (..),
    GenericDotOptics (..),
    GenericDotOpticsMethod,
    FieldDotOptics (..),
    FieldDotOpticsMethod,
    -- * Things that will eventually be in base
    SetField (..),
  )
where

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

-- | Helper typeclass, used only to specify the method for deriving dot optics.
-- Usually derived with @DerivingVia@.
--
-- See 'GenericDotOptics' and 'FieldDotOptics'.
class DotOptics s where
  type DotOpticsMethod s :: Type

-- | Produce an optic according to the given method.
--
-- The @name v -> u a b@ fundep could be added but doesn't seem to be necessary.
-- Could it improve type inference?
type HasDotOptic :: Type -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class HasDotOptic method name u v a b | name u -> a b where
  dotOptic :: Lens u v a b

data GenericDotOpticsMethod

-- | For deriving 'DotOptics' using DerivingVia. The wrapped type is not used for anything.
--
-- Supports type-changing updates.
newtype GenericDotOptics s = GenericDotOptics s

instance DotOptics (GenericDotOptics s) where
  type DotOpticsMethod (GenericDotOptics s) = GenericDotOpticsMethod

-- | Produce an optic using the optics' package own generic machinery.
instance
  (GField name s t a b) =>
  HasDotOptic GenericDotOpticsMethod name s t a b
  where
  dotOptic = gfield @name

data FieldDotOpticsMethod

-- | For deriving 'DotOptics' using DerivingVia. The wrapped type is not used for anything.
--
-- Does not support type-changing updates.
newtype FieldDotOptics s = FieldDotOptics s

instance DotOptics (FieldDotOptics s) where
  type DotOpticsMethod (FieldDotOptics s) = FieldDotOpticsMethod

-- | Produce an optic using the 'HasField'/'SetField' machinery form "GHC.Records".
instance
  ( HasField name s a,
    SetField name s a,
    s ~ t,
    a ~ b
  ) =>
  -- if you change to @name s s a a@, a compilation error crops up in tests.
  HasDotOptic FieldDotOpticsMethod name s t a b
  where
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))

-- | Identity 'Iso'. Used as a starting point for dot access. It's a renamed 'Optics.Core.equality'.
the :: Iso s t s t
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r

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