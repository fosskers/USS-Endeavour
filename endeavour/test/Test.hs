{-# LANGUAGE OverloadedStrings #-}

import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import qualified Data.Map.Lazy as M
import           Data.Text (Text)
import           Database.SQLite.Simple
import           Endeavour.Genetics
import           Endeavour.Knowledge.Hue
import           Endeavour.Knowledge.LittleBits.Internal
import           Endeavour.Memory
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = do
  c <- open ":memory:"
  env <- awaken "/home/colin/code/haskell/endeavour/config.yml"
  case env of
    Nothing -> putStrLn "Couldn't read config.yml" >> close c
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
  , testGroup "Phillips Hue Lights"
    [ testCase "Light count" $ lightsT env
    , testCase "L2 Brightness" $ briT env
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
  r @?= Right (CBStatus "Computer" (_deviceId $ _cloudbit e) 135545 False)
  where f :: IO (Either Text CBStatus)
        f = runLift . runExc $ runReader status e

-- | A test for any `ERL` function that returns `()`.
runT :: Env -> Effect () -> Assertion
runT env eff = f >>= assertRight "Crap"
  where f :: IO (Either Text ())
        f = runLift . runExc $ runReader eff env

outputT :: Env -> Assertion
outputT e = runT e . emit $ CBOutput 100 3000

briT :: Env -> Assertion
briT e = runT e $ lightOn 2 >> lightBri 128 2 >> lightBri 200 2 >> lightOff 2

ioIso :: Env -> Assertion
ioIso = runLift . runReader f
  where f = do
          reader _conn >>= lift . wake
          chronicle Info "chronicle"
          (Log _ cat t : _) <- recall Nothing
          lift ((cat, t) @?= (Info, "chronicle"))

lightsT :: Env -> Assertion
lightsT e = do
  r <- fmap (\m -> M.size <$> m) f
  r @?= Right 3
  where f :: IO (Either Text (M.Map Text Light))
        f = runLift . runExc $ runReader lights e
