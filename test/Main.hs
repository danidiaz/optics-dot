{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import GHC.Records
import GHC.Generics
import Optics.Core
import Optics.Lens.Dot
import GHC.TypeLits

data Whole a = Whole {
    whole1 :: Int, 
    part :: Part a
} deriving stock Generic

data Part a = Part {
    part1 :: Bool,
    subpart :: Subpart a  
} deriving stock Generic

data Subpart a = Subpart {
    subpart1 :: String,
    foo :: a
} deriving stock Generic

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7))

whole' :: Whole Bool
whole' = whole & the.part.subpart.foo .~ False 

main :: IO ()
main = putStrLn "Test suite not yet implemented."
