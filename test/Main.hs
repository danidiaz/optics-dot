{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DerivingVia #-}

-- https://stackoverflow.com/questions/53009549/haskell-derivingvia-on-multi-param-type-classes-with-fun-deps
module Main (main) where

import GHC.Generics
import GHC.Records
import Optics.Core
import Optics.Dot

data Whole z = Whole
  { whole1 :: Int,
    part :: Part z
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics (Whole z)) instance GField name (Whole z) (Whole p) a b => RecordDotOptics name (Whole z) (Whole p) a b (Whole z)

-- instance (GField name (Whole p) (Whole q) a b) => RecordDotOptics name (Whole p) (Whole q) a b (Whole p) where
--   dotOptic = gfield @name

data Part z = Part
  { part1 :: Bool,
    subpart :: Subpart z
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics (Part z)) instance GField name (Part z) (Part p) a b => RecordDotOptics name (Part z) (Part p) a b (Part z)

-- instance (GField name (Part p) (Part q) a b) => RecordDotOptics name (Part p) (Part q) a b (Part p) where
--   dotOptic = gfield @name

data Subpart z = Subpart
  { wee :: String,
    foo :: z,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics (Subpart z)) instance GField name (Subpart z) (Subpart p) a b => RecordDotOptics name (Subpart z) (Subpart p) a b (Subpart z)

-- instance (GField name (Subpart p) (Subpart q) a b) => RecordDotOptics name (Subpart p) (Subpart q) a b (Subpart p) where
--   dotOptic = gfield @name


data YetAnotherSubpart = YetAnotherSubpart
  { ooo :: String,
    uuu :: Int
  }
  deriving (Show)

-- -- | 'YetAnotherSubpart' doesn't use the 'GField' machinery for
-- -- 'RecordDotOptics'. Instead, it uses 'HasField'/'SetField'. Field-changing
-- -- updates are not supported here.
-- instance
--   (HasField name YetAnotherSubpart x, SetField name YetAnotherSubpart x) =>
--   RecordDotOptics name x x YetAnotherSubpart YetAnotherSubpart
--   where
--   dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))
-- 
-- instance SetField "ooo" YetAnotherSubpart String where
--   setField ooo r = r {ooo}

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))

-- | Note the the type-changing update
wholex :: Whole Bool
wholex = whole & the.part .~ Part True (Subpart "wee" True (YetAnotherSubpart "oldval" 3))

-- -- | Note the the type-changing update
-- whole' :: Whole Bool
-- whole' = whole & the.part.subpart.foo .~ False

-- -- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
-- whole'' :: Whole Int
-- whole'' = whole & the.part.subpart.yet.ooo .~ "newval"

main :: IO ()
main = do
  print whole
  -- print whole'
  -- print whole''
