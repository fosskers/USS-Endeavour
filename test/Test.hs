{-# LANGUAGE OverloadedStrings #-}

import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Data.Text (Text)
import Database.SQLite.Simple
import Endeavour.Genetics
import Endeavour.Knowledge.LittleBits.Internal
import Endeavour.Memory
import Test.Tasty
import Test.Tasty.HUnit

---

main :: IO ()
main = do
  c <- open ":memory:"
  env <- genes "/home/colin/code/haskell/endeavour/config.json"
  case env of
    Nothing -> putStrLn "Couldn't read config.json" >> close c
    Just e  -> do
      close $ _conn e
      defaultMain $ suite (e { _conn = c })
      close c

suite :: Env -> TestTree
suite env = testGroup "Endeavour System Logic Diagnostic"
  [ testGroup "Memory Cores"
    [ testCase "IO Isomorphism" $ ioIso env
    ]
  , testGroup "LittleBits Relays"
    [ testCase "Endpoint: devices/ID" $ statusT env
    , testCase "Endpoint: devices/ID/output" $ outputT env
    ]
  ]

assertRight :: String -> Either a b -> Assertion
assertRight _ (Right _)  = return ()
assertRight msg (Left _) = assertFailure msg

instance Assertable (Either a b) where
  assert = assertRight ""

statusT :: Env -> Assertion
statusT e = do
  r <- f
  r @?= Right (CBStatus "Computer" (_deviceId $ _cloudbit e) 135545 True)
  where f :: IO (Either Text CBStatus)
        f = runLift . runExc $ runReader status e

outputT :: Env -> Assertion
outputT e = do
  r <- f
  assert True
  where f :: IO (Either Text ())
        f = runLift . runExc $ runReader (emit $ CBOutput 50 3000) e

ioIso :: Env -> Assertion
ioIso = runLift . runReader f
  where f = do
          wake
          chronicle Info "chronicle"
          ((Log _ cat t):_) <- recall Nothing
          lift ((cat, t) @?= (Info, "chronicle"))
