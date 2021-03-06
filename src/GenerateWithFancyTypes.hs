{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module GenerateWithFancyTypes where

import Data.SBV ( free, sTrue, SBV, SBool, Symbolic )
import Data.HList.CommonMain ( HList(..) )

pattern (:>) :: x -> HList xs -> HList (x ': xs)
pattern h :> t = HCons h t
infixr 5 :>

data G a where
  GInt :: String -> G Integer
  GString :: String -> G String
  GBool :: String -> G Bool
deriving instance Show (G a)

data Types a where
  TypesList :: Types [Int, String, Bool]

raw :: HList [G Integer, G Bool, G Integer]
raw = GInt "A : Int" :>  GBool "B : Bool" :> GInt "C : Int" :> HNil

raw2 :: [String]
raw2 = ["Int", "Bool"]

class ParseRaw2 a where
  parseRaw2 :: [String] -> HList a
instance ParseRaw2 '[] where
  parseRaw2 [] = HNil
  parseRaw2 _ = error "bad parse"


-- parseRaw2 :: Types [Int, String, Bool] -> String -> G a
-- parseRaw2 _ = \case
--   "Int" -> GInt "An Integer"
--   "Bool" -> GBool "A Bool"
--   "String" -> GString "A String"


selection :: [Bool]
selection = [True, False, True]

mkSymb :: G a -> Symbolic (SBV a)
mkSymb = \case
  GInt s -> free s
  GString s -> free s
  GBool s -> free s

data EHList = forall a. Show (HList a) => EHList (HList a)
-- data EHList where
--   EHList :: HList a -> EHList
deriving instance Show EHList

-- instance Show (HList '[])

--- | Doesn't work because HList is a data family, not a GADT
-- gFilter :: HList a -> [Bool] -> EHList
-- gFilter HNil _ = EHList HNil
-- gFilter xs [] = EHList xs
-- gFilter (HCons x xs) (b:bs) =
--   case gFilter xs bs of
--     EHList l -> if b then EHList (HCons x l) else EHList l

class GFilter a where
  gFilter :: HList a -> [Bool] -> EHList

instance GFilter '[] where
  gFilter HNil _ = EHList HNil
instance (GFilter xs, Show x, Show (HList xs)) => GFilter (x ': xs) where
  gFilter xs [] = EHList xs
  gFilter (HCons x xs) (b:bs) =
    case gFilter xs bs of
      EHList l -> if b then EHList (HCons x l) else EHList l

question :: GFilter a => HList a -> [Bool] -> Symbolic SBool
question xs _bs =
  let _relevant = gFilter xs
  in do
    return sTrue

