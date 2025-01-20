module Main where

import TestData
import Lean.Eval
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = if testAll then exitSuccess else exitFailure

testAll :: Bool
testAll =
  and [ testBools
      , testNat
      , testList
      ]

testBools :: Bool
testBools =
  and [ -- Not
        eval n boolDef [t] == f
      , eval n boolDef [f] == t
        -- And
      , eval a boolDef [t,t] == t
      , eval a boolDef [t,f] == f
      , eval a boolDef [f,t] == f
      , eval a boolDef [f,f] == f
        -- Or
      , eval o boolDef [t,t] == t
      , eval o boolDef [t,f] == t
      , eval o boolDef [f,t] == t
      , eval o boolDef [f,f] == f
      ]

testNat :: Bool
testNat =
  and [ eval add natDef [zero,zero] == zero
      , eval add natDef [zero,one]  == one
      , eval add natDef [one, zero] == one
      , eval add natDef [one, one]  == two
      , eval add natDef [intToValPeano 367, intToValPeano 22] == intToValPeano (367 + 22)
      ]

testList :: Bool
testList =
  and [ eval app listDef [emp,emp] == emp
      , eval app listDef [emp,som] == som
      , eval app listDef [som,emp] == som
      , eval rev listDef [emp]  == emp
      , eval rev listDef [som]  == som'
      , eval rev listDef [som'] == som
      , eval rev listDef [eval rev listDef [som]]  == som
      , eval rev listDef [eval rev listDef [som']] == som'
      ]

testPair :: Bool
testPair =
  and [ eval swap pairDef [mkPair True zero t] == mkPair False t zero ]
