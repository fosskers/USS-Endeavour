{-# LANGUAGE OverloadedStrings #-}

import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Eff.Exception
import Database.SQLite.Simple
import Endeavour.Memory
import Endeavour.Types
import Endeavour.Knowledge.LittleBits.Internal
import Test.Tasty
import Test.Tasty.HUnit
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text (Text)

---

main :: IO ()
main = do
  c <- open ":memory:"
  m <- newManager tlsManagerSettings
  let env = Env c (CloudBit "" "") m
  defaultMain $ suite env
  close c

suite :: Env -> TestTree
suite env = testGroup "Endeavour System Logic Diagnostic"
  [ testGroup "Memory Cores"
    [ testCase "IO Isomorphism" $ ioIso env
    ]
  , testGroup "LittleBits Relays"
    [ testCase "Endpoint: devices/" $ doIt env
    ]
  ]

assertRight :: String -> Either a b -> Assertion
assertRight _ (Right _)  = return ()
assertRight msg (Left _) = assertFailure msg

instance Assertable (Either a b) where
  assert = assertRight ""

doIt :: Env -> Assertion
doIt e = do
  r <- f
  assert r
  where f :: IO (Either Text Status)
        f = runLift . runExc $ runReader (transmit devices) e

ioIso :: Env -> Assertion
ioIso = runLift . runReader f
  where f = do
          wake
          chronicle Info "chronicle"
          ((Log _ cat t):_) <- recall Nothing
          lift ((cat, t) @?= (Info, "chronicle"))
