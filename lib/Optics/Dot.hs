{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeData #-}
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
-- Except when some other optics already serves as a starting point:
--
-- >>> :{
-- nonTypChanging2 :: [Whole Int]
-- nonTypChanging2 = [whole, whole] & traversed.part.subpart.yet.ooo .~ "newval"
-- :}
--
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
-- We can refer to constructors of sum types. Note how 'DotOptics' is derived via 'GenericConstructors':
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
-- For more advanced cases, we can manually define 'DotOptics' and 'HasDotOptic' instances
-- for our datatypes, instead of deriving them.
--
-- >>> :{
-- data Wee = Wee { vvv :: Int, bbb :: Int }
--    deriving stock (Show, Generic)
--    deriving (DotOptics) via CustomOptics Wee
-- instance HasDotOptic CustomOptics "vvv" A_Lens NoIx Wee Wee Int Int where
--    dotOptic = lens (.vvv) (\r vvv -> r { vvv })
-- :}
module Optics.Dot
  ( the,
    DotOptics (..),
    HasDotOptic (..),
    GenericFields (..),
    GenericAffineFields (..),
    GenericConstructors (..),
    GenericConstructorsAndAffineFields (..),
    CustomOptics (..),
  )
where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( DotOptics u,
    method u ~ DotOpticsMethod u,
    HasDotOptic method dotName l js u v a b,
    JoinKinds k l m,
    AppendIndices is js ks
  ) =>
  HasField dotName (Optic k is s t u v) (Optic m ks s t a b)
  where
  -- \| Compare with the signature of '(%)'.
  getField o = o % (dotOptic @method @dotName @l @js @u @v @a @b)

-- | Helper typeclass, used to specify the method for deriving dot optics.
-- Usually derived with @DerivingVia@.
--
-- See 'GenericFields', 'GenericAffineFields', 'GenericConstructors',
-- 'GenericConstructorsAndAffineFields', 'CustomOptics'.
class DotOptics s where
  type DotOpticsMethod s :: Type

-- | Produce an optic according to the given method.
type HasDotOptic :: (Type -> Type) -> Symbol -> OpticKind -> IxList -> Type -> Type -> Type -> Type -> Constraint
class
  HasDotOptic method dotName k is s t a b
    | dotName s -> t a b k is,
      dotName t -> s a b k is
  where
  dotOptic :: Optic k is s t a b

type GenericFieldsMethod :: Type -> Type
data GenericFieldsMethod s

-- | For deriving 'DotOptics' using @DerivingVia@.
--
-- Supports type-changing updates.
--
-- The wrapped type must have a 'Generic' instance.
newtype GenericFields s = MakeGenericFields s

instance DotOptics (GenericFields s) where
  type DotOpticsMethod (GenericFields s) = GenericFieldsMethod s

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GField dotName s t a b,
    k ~ A_Lens,
    is ~ NoIx
  ) =>
  HasDotOptic GenericFieldsMethod dotName k is s t a b
  where
  dotOptic = gfield @dotName

type GenericAffineFieldsMethod :: Type -> Type
data GenericAffineFieldsMethod s

-- | For deriving 'DotOptics' using @DerivingVia@.
--
-- Supports type-changing updates.
--
-- The wrapped type must have a 'Generic' instance.
newtype GenericAffineFields s = MakeGenericAffineFields s

instance DotOptics (GenericAffineFields s) where
  type DotOpticsMethod (GenericAffineFields s) = GenericAffineFieldsMethod s

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GAffineField dotName s t a b,
    k ~ An_AffineTraversal,
    is ~ NoIx
  ) =>
  HasDotOptic GenericAffineFieldsMethod dotName k is s t a b
  where
  dotOptic = gafield @dotName

-- | For deriving 'DotOptics' using @DerivingVia@. The wrapped type is not used for anything.
--
-- Supports type-changing updates.
newtype GenericConstructors s = MakeGenericConstructors s

instance DotOptics (GenericConstructors s) where
  type DotOpticsMethod (GenericConstructors s) = GenericConstructors s

-- | Produce an optic using the optics' package own generic machinery.
instance
  ( GConstructor constructorName s t a b,
    -- Dot notation doesn't allow starting with uppercase like constructors do, so we prepend an underscore.
    dotName ~ ConsSymbol '_' constructorName,
    k ~ A_Prism,
    is ~ NoIx
  ) =>
  HasDotOptic GenericConstructors dotName k is s t a b
  where
  dotOptic = gconstructor @constructorName

type data DotNameForWhat
  = ConstructorDotName
  | FieldDotName

-- | Type family to check if a symbol starts with an underscore.
type family AnalyzeDotName (s :: Symbol) :: (Symbol, DotNameForWhat) where
  AnalyzeDotName s = AnalyzeDotNameHelper s (UnconsSymbol s)

type family AnalyzeDotNameHelper (original :: Symbol) (m :: Maybe (Char, Symbol)) :: (Symbol, DotNameForWhat) where
  AnalyzeDotNameHelper original ('Just '( '_', rest)) = '(rest, ConstructorDotName)
  AnalyzeDotNameHelper original _ = '(original, FieldDotName)

-- | Helper typeclass that dispatches based on whether the name starts with underscore.
class
  HasConstructorOrAffineFieldOptic (nameAnalysis :: (Symbol, DotNameForWhat)) (k :: OpticKind) (is :: IxList) s t a b
    | nameAnalysis s -> t a b k is,
      nameAnalysis t -> s a b k is
  where
  dotOpticHelper :: Optic k is s t a b

instance
  ( GConstructor name s t a b,
    k ~ A_Prism,
    is ~ NoIx
  ) =>
  HasConstructorOrAffineFieldOptic '(name, ConstructorDotName) k is s t a b
  where
  dotOpticHelper = gconstructor @name

instance
  ( GAffineField name s t a b,
    k ~ An_AffineTraversal,
    is ~ NoIx
  ) =>
  HasConstructorOrAffineFieldOptic '(name, FieldDotName) k is s t a b
  where
  dotOpticHelper = gafield @name

-- | For deriving 'DotOptics' using @DerivingVia@.
--
-- This combines constructors (names starting with '_') and affine fields (other names).
--
-- Supports type-changing updates.
--
-- The wrapped type must have a 'Generic' instance.
--
-- >>> :{
-- data Branchy =
--      SomeBranch { foo :: Int, bar :: Bool }
--    | OtherBranch { wee :: String }
--    deriving stock (Generic, Show)
--    deriving (DotOptics) via GenericConstructorsAndAffineFields Branchy
-- branchy :: Branchy
-- branchy = SomeBranch { foo = 0, bar = False }
-- --
-- fooVal :: Maybe Int
-- fooVal = branchy ^? the.foo
-- branchVal :: Maybe String
-- branchVal = branchy ^? the._OtherBranch
-- :}
newtype GenericConstructorsAndAffineFields s = MakeGenericConstructorsAndAffineFields s

instance DotOptics (GenericConstructorsAndAffineFields s) where
  type DotOpticsMethod (GenericConstructorsAndAffineFields s) = GenericConstructorsAndAffineFields s

-- | Produce an optic using the optics' package own generic machinery.
-- Delegates to GConstructor or GAffineField depending on whether dotName starts with '_'.
instance
  ( nameAnalysis ~ AnalyzeDotName dotName,
    '(name, dotNameForWhat) ~ nameAnalysis,
    HasConstructorOrAffineFieldOptic nameAnalysis k is s t a b
  ) =>
  HasDotOptic GenericConstructorsAndAffineFields dotName k is s t a b
  where
  dotOptic = dotOpticHelper @(AnalyzeDotName dotName)

newtype CustomOptics s = MakeCustomOptics s

instance DotOptics (CustomOptics s) where
  type DotOpticsMethod (CustomOptics s) = CustomOptics s

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
