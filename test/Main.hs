{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import GHC.Generics
import GHC.Records
import Optics.Core
import Optics.Dot
import Data.Kind (Type, Constraint)

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic, Show)
  deriving (DotOptics) via GenericFields (Whole a)

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)
  deriving (DotOptics) via GenericFields (Part a)

data Subpart a = Subpart
  { wee :: String,
    foo :: a,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)
  deriving (DotOptics) via GenericFields (Subpart a)

data YetAnotherSubpart = YetAnotherSubpart
  { ooo :: String,
    uuu :: Int
  }
  deriving (Show)
  deriving (DotOptics) via Fields YetAnotherSubpart


-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r

-- | 'YetAnotherSubpart' doesn't use the 'GField' machinery for
-- 'RecordDotOptics'. Instead, it uses 'HasField'/'SetField'. Field-changing
-- updates are not supported here.
instance SetField "ooo" YetAnotherSubpart String where
  setField ooo r = r {ooo}

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))

typChanging1 :: Whole Bool
typChanging1 = whole & the.part .~ Part True (Subpart "wee" False (YetAnotherSubpart "oldval" 3))

typChanging2 :: Whole Bool
typChanging2 = whole & the.part.subpart .~ Subpart "wee" False (YetAnotherSubpart "oldval" 3)

typeChanging3 :: Whole String
typeChanging3 = whole & the.part.subpart .~ Subpart "wee" "stuff" (YetAnotherSubpart "oldval" 3)

typeChanging4 :: Whole String
typeChanging4 = whole & the.part.subpart.foo .~ "stuff"

-- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
nonTypChanging1 :: Whole Int
nonTypChanging1 = whole & the.part.subpart.yet.ooo .~ "newval"

normalDotAccess :: String
normalDotAccess = whole.part.subpart.yet.ooo

data Animal a
  = Dog {name :: String, age :: Int}
  | Cat {name :: String, purrs :: Bool}
  | Squirrel { twees :: Bool}
  | Octopus {tentacles :: Whole a}
  deriving (Show, Generic)
  deriving (DotOptics) via GenericConstructors (Animal a)

dog :: Animal Int
dog = Dog {name = "Fido", age = 5}

matchesDog :: Maybe ([Char], Int)
matchesDog = dog ^? the._Dog

matchesSquirrel :: Maybe Bool
matchesSquirrel = dog ^? the._Squirrel

changesOctopus :: Animal Bool
changesOctopus = dog & the._Octopus.part.subpart.foo .~ False

data Carta a = 
      Sota | Caballo | Rey {valor :: a}
  deriving (Show, Generic)
  deriving (DotOptics) via GenericAffineFields (Carta a)

carta :: Carta Int
carta = Rey { valor = 3 }

cartaRey :: Carta Bool
cartaRey = carta & the.valor .~ True


data FieldsMethod

-- | For deriving 'DotOptics' using DerivingVia. The wrapped type is not used for anything.
--
-- Doesn't support type-changing updates.
newtype Fields s = MakeFields s

instance DotOptics (Fields s) where
  type DotOpticsMethod (Fields s) = FieldsMethod

-- | Produce an optic using the 'HasField'/'SetField' machinery form "GHC.Records".
instance
  ( HasField name s a,
    SetField name s a,
    s ~ t,
    a ~ b,
    name ~ dotName
  ) =>
  -- if you change to @name s s a a@, a compilation error crops up in tests.
  HasDotOptic FieldsMethod name dotName s t a b
  where
  type DotOpticKind FieldsMethod name s = A_Lens
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))

main :: IO ()
main = do
  print whole
  print typChanging1
  print typChanging2
  print typeChanging3
  print typeChanging4
  print nonTypChanging1
  print normalDotAccess
  print matchesDog
  print matchesSquirrel
  print changesOctopus
  print cartaRey
