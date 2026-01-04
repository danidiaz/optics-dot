{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import GHC.Generics
import Optics.Core
import Optics.Dot

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic, Show)
  deriving (HasOpticsMethod) via (GenericsDotOptics (Whole a))

-- instance HasOpticsMethod (Whole s) where
--   type Method (Whole s) = GenericsDotOptics

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)
  deriving (HasOpticsMethod) via (GenericsDotOptics (Part a))

-- instance HasOpticsMethod (Part s) where
--   type Method (Part s) = GenericsDotOptics

data Subpart a = Subpart
  { wee :: String,
    foo :: a,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)
  deriving (HasOpticsMethod) via (GenericsDotOptics (Subpart a))

-- instance HasOpticsMethod (Subpart s) where
--   type Method (Subpart s) = GenericsDotOptics

data YetAnotherSubpart = YetAnotherSubpart
  { ooo :: String,
    uuu :: Int
  }
  deriving (Show)
  deriving (HasOpticsMethod) via (FieldDotOptics YetAnotherSubpart)

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

-- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
nonTypChanging1 :: Whole Int
nonTypChanging1 = whole & the.part.subpart.yet.ooo .~ "newval"

normalDotAccess :: String
normalDotAccess = whole.part.subpart.yet.ooo

main :: IO ()
main = do
  print whole
  print typChanging1
  print typChanging2
  print typeChanging3
  print nonTypChanging1
  print normalDotAccess
