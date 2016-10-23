{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Proxy
import           Endeavour.Genetics
import           Endeavour.Knowledge.LittleBits.Internal
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Servant.API
import           Servant.Server

---

-- | Command-line arguments.
data Args = Args { config :: FilePath } deriving (Generic)

instance ParseRecord Args

type API = "cb" :> Capture "power" Int :> Capture "duration" Int :> Get '[JSON] ()

api :: Proxy API
api = Proxy

-- | The request handler functions, all of which operate in the `Effect` Monad.
serverT :: ServerT API Effect
serverT = emit

effToHandler' :: Env -> Effect a -> Handler a
effToHandler' env eff = liftIO (runLift . runExc $ runReader eff env) >>= f
  where f = either (\_ -> throwE err404) pure

effToHandler :: Env -> (Effect :~> Handler)
effToHandler e = Nat (effToHandler' e)

server :: Env -> Server API
server e = enter (effToHandler e) serverT

app :: Env -> Application
app = serve api . server

main :: IO ()
main = do
  Args c <- getRecord "U.S.S. Endeavour - Computing Core"
  env <- awaken c
  case env of
    Nothing -> putStrLn "Failed to parse config file."
    Just e  -> do
      putStrLn "U.S.S. Endeavour - Computing Core activated."
      W.run 8081 $ app e  -- How do we ever exit this?
      slumber e
