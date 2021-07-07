-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module MarysIdeal where

import Data.SBV
import Data.SBV.Control (Query, getValue, CheckSatResult (Sat), query, checkSat)
import Data.Functor (($>))

-- reference example
-- https://hackage.haskell.org/package/sbv-8.15/docs/src/Documentation.SBV.Examples.Puzzles.Murder.html#Location
-- alternatively https://github.com/LeventErkok/sbv/blob/master/Documentation/SBV/Examples/Puzzles/Murder.hs

data Person f = Person
  { nm :: String,
    tall :: f Bool,
    dark :: f Bool,
    handsome :: f Bool
  }

-- | Helper functor
newtype Const a = Const { getConst :: a }

-- | Show a person
instance Show (Person Const) where
  show (Person n t d h) = unwords [n, show (getConst t), show (getConst d), show (getConst h)]

-- | Create a new symbolic person
newPerson :: String -> Symbolic (Person SBV)
newPerson n = 
  Person n <$> free (n ++ "Tall") <*> free (n ++ "Dark") <*> free (n ++ "Handsome")

-- | Get the concrete value of the person in the model
getPerson :: Person SBV -> Query (Person Const)
getPerson Person{nm, tall, dark, handsome} =
  Person nm <$> (Const <$> getValue tall)
            <*> (Const <$> getValue dark)
            <*> (Const <$> getValue handsome)

-- Mary's ideal man is tall, dark, and handsome.
ideal :: Person SBV -> SBool
ideal Person{..} = sAnd [tall, dark, handsome]


puzzle :: Symbolic [Person Const]
puzzle = do
  -- She knows four men: Alec, Bill, Carl, and Dave.
  alec    <- newPerson "Alec"
  bill  <- newPerson "Bill"
  carl  <- newPerson "Carl"
  dave <- newPerson "Dave"

  let abcd = [alec, bill, carl, dave]

  -- Only one of the four men has all of the characteristics Mary requires.
  constrain $ pbExactly (map ideal abcd) 1

  -- Only three of the men are tall, only two are dark, and only one is handsome.
  constrain $ pbExactly (map tall abcd) 3
  constrain $ pbExactly (map dark abcd) 2
  constrain $ pbExactly (map handsome abcd) 1

  -- Each of the four men has at least one of the required traits.
  let atLeastOneTrait Person{..} = pbAtLeast [tall, dark, handsome] 1
  constrain $ sAll atLeastOneTrait abcd
  -- Alec and Bill have the same complexion.
  constrain $ dark alec .<=> dark bill
  
  -- Bill and Carl are the same height.
  constrain $ tall bill .<=> tall carl

  -- Carl and Dave are not both tall.  
  constrain $ sNot $ tall carl .&& tall dave

  query $ do
    cs <- checkSat
    case cs of
      Sat -> do
        traverse getPerson abcd
      _ -> error $ "Solver said: " ++ show cs

main :: IO ()
main = runSMT puzzle >>= print

mainVerbose :: IO ()
mainVerbose = runSMTWith z3{verbose = True} puzzle >>= print

mainOutput :: IO ()
mainOutput = runSMTWith z3{transcript = Just "marysIdeal_z3-generated.smtlib"} puzzle >>= print

{-
*MarysIdeal> main
[Alec True False False,Bill True False False,Carl True True True,Dave False True False]
-}

--------------------------------

--   return []

-- mainAllSat :: IO ()
-- mainAllSat = allSat (puzzle $> sTrue) >>= print

{-
*MarysIdeal> mainAllSat 
Solution #1:
  AlecTall     =  True :: Bool
  AlecDark     = False :: Bool
  AlecHandsome = False :: Bool
  BillTall     =  True :: Bool
  BillDark     = False :: Bool
  BillHandsome = False :: Bool
  CarlTall     =  True :: Bool
  CarlDark     =  True :: Bool
  CarlHandsome =  True :: Bool
  DaveTall     = False :: Bool
  DaveDark     =  True :: Bool
  DaveHandsome = False :: Bool
This is the only solution.
-}

--------------------------------

-- Alloy output
-- https://github.com/smucclaw/sandbox/blob/default/joeylim/MarysIdeal.als
-- https://alloytools.org/download.html
-- https://github.com/AlloyTools/org.alloytools.alloy/releases

{-
┌───────────────┐
│this/Complexion│
├───────────────┤
│Dark$0         │
├───────────────┤
│Pale$0         │
└───────────────┘

┌──────────┐
│this/Looks│
├──────────┤
│Average$0 │
├──────────┤
│Handsome$0│
└──────────┘

┌────────┬───────┬──────────┬──────────┐
│this/Man│height │complexion│looks     │
├────────┼───────┼──────────┼──────────┤
│A$0     │Tall$0 │Pale$0    │Average$0 │
├────────┼───────┼──────────┼──────────┤
│B$0     │Tall$0 │Pale$0    │Average$0 │
├────────┼───────┼──────────┼──────────┤
│C$0     │Tall$0 │Dark$0    │Handsome$0│
├────────┼───────┼──────────┼──────────┤
│D$0     │Short$0│Dark$0    │Average$0 │
└────────┴───────┴──────────┴──────────┘

┌───────────┐
│this/Height│
├───────────┤
│Short$0    │
├───────────┤
│Tall$0     │
└───────────┘
-}
