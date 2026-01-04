{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

-- https://stackoverflow.com/questions/53009549/haskell-derivingvia-on-multi-param-type-classes-with-fun-deps
module Main (main) where

import GHC.Generics
import GHC.Records
import Optics.Core
import Optics.Dot

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic, Show)

instance HasOpticsMethod (Whole a) where
  type Method (Whole a)  = GenericsDotOptics

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)

instance HasOpticsMethod (Part a) where
  type Method (Part a) = GenericsDotOptics

data Subpart a = Subpart
  { wee :: String,
    foo :: a,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)

instance HasOpticsMethod (Subpart a)  where
  type Method (Subpart a) = GenericsDotOptics

data YetAnotherSubpart = YetAnotherSubpart
  { ooo :: String,
    uuu :: Int
  }
  deriving (Show)

-- | 'YetAnotherSubpart' doesn't use the 'GField' machinery for
-- 'RecordDotOptics'. Instead, it uses 'HasField'/'SetField'. Field-changing
-- updates are not supported here.
-- instance
--   ( HasField name YetAnotherSubpart x,
--     SetField name YetAnotherSubpart x
--   ) =>
--   RecordDotOptics name YetAnotherSubpart YetAnotherSubpart x x
--   where
--   dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))

instance SetField "ooo" YetAnotherSubpart String where
  setField ooo r = r {ooo}

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))


whole'1 :: Whole Bool
whole'1 = whole & the.part .~ (Part True (Subpart "wee" False (YetAnotherSubpart "oldval" 3)))

-- | Note the the type-changing update
whole' :: Whole Bool
whole' = whole & the.part.subpart .~ (Subpart "wee" False (YetAnotherSubpart "oldval" 3))

-- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
-- whole'' :: Whole Int
-- whole'' = whole & the.part.subpart.yet.ooo .~ "newval"

-- normalDotAccess :: String
-- normalDotAccess = whole.part.subpart.yet.ooo

main :: IO ()
main = do
  print whole
  print whole'
  -- print whole''
  -- print normalDotAccess
