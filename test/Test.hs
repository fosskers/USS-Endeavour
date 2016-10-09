{-# LANGUAGE OverloadedStrings #-}

import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Data.Text (Text)
import Database.SQLite.Simple
import Endeavour.Knowledge.LittleBits.Internal
import Endeavour.Memory
import Endeavour.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Tasty
import Test.Tasty.HUnit

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
  r @?= Right (CBStatus "foo" "bar" 1 True)
  where f :: IO (Either Text CBStatus)
        f = runLift . runExc $ runReader status e

ioIso :: Env -> Assertion
ioIso = runLift . runReader f
  where f = do
          wake
          chronicle Info "chronicle"
          ((Log _ cat t):_) <- recall Nothing
          lift ((cat, t) @?= (Info, "chronicle"))
