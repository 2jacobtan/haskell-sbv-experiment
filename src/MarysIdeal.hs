-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module MarysIdeal where

import Data.SBV
    ( free,
      runSMT,
      runSMTWith,
      (.&&),
      (.<=>),
      sAll,
      sAnd,
      sNot,
      pbAtLeast,
      pbExactly,
      z3,
      SBV,
      SBool,
      constrain,
      SMTConfig(verbose, transcript),
      Symbolic )
import Data.SBV.Control (Query, getValue, CheckSatResult (Sat), query, checkSat)
import Data.Functor (($>))
import Text.Pretty.Simple ( pPrint )
import Data.Functor.Identity ( Identity(Identity) )

-- reference example
-- https://hackage.haskell.org/package/sbv-8.15/docs/src/Documentation.SBV.Examples.Puzzles.Murder.html#Location
-- alternatively https://github.com/LeventErkok/sbv/blob/master/Documentation/SBV/Examples/Puzzles/Murder.hs

data Person f = Person
  { nm :: String,
    tall :: f Bool,
    dark :: f Bool,
    handsome :: f Bool
  }

-- | Show a person
deriving instance Show (Person Identity)
-- instance Show (Person Identity) where
--   show (Person n t d h) = unwords $ n : map (show . runIdentity) [t, d, h]

-- | Create a new symbolic person
newPerson :: String -> Symbolic (Person SBV)
newPerson n = 
  Person n <$> free (n ++ "Tall") <*> free (n ++ "Dark") <*> free (n ++ "Handsome")

-- | Get the concrete value of the person in the model
getPerson :: Person SBV -> Query (Person Identity)
getPerson Person{nm, tall, dark, handsome} =
  Person nm <$> (Identity <$> getValue tall)
            <*> (Identity <$> getValue dark)
            <*> (Identity <$> getValue handsome)

-- Mary's ideal man is tall, dark, and handsome.
ideal :: Person SBV -> SBool
ideal Person{..} = sAnd [tall, dark, handsome]


puzzle :: Symbolic [Person Identity]
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
main = runSMT puzzle >>= pPrint

mainVerbose :: IO ()
mainVerbose = runSMTWith z3{verbose = True} puzzle >>= print

mainOutput :: IO ()
mainOutput = runSMTWith z3{transcript = Just "marysIdeal_z3-generated.smtlib"} puzzle >>= print

{-
*MarysIdeal PPrint> main
[ Person
    { nm = "Alec"
    , tall = Identity True
    , dark = Identity False
    , handsome = Identity False
    }
, Person
    { nm = "Bill"
    , tall = Identity True
    , dark = Identity False
    , handsome = Identity False
    }
, Person
    { nm = "Carl"
    , tall = Identity True
    , dark = Identity True
    , handsome = Identity True
    }
, Person
    { nm = "Dave"
    , tall = Identity False
    , dark = Identity True
    , handsome = Identity False
    }
]
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
