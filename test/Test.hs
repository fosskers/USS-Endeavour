{-# LANGUAGE OverloadedStrings #-}

import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Database.SQLite.Simple
import Endeavour.Memory
import Endeavour.Types
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = do
  c <- open ":memory:"
  let env = Env c
  defaultMain $ suite env
  close c

suite :: Env -> TestTree
suite env = testGroup "Endeavour System Logic Diagnostic"
  [ testGroup "Memory Cores"
    [ testCase "IO Isomorphism" $ ioIso env
    ]
  ]

ioIso :: Env -> Assertion
ioIso = runLift . runReader f
  where f = do
          wake
          chronicle Info "chronicle"
          ((Log _ cat t):_) <- recall Nothing
          lift ((cat, t) @?= (Info, "chronicle"))
