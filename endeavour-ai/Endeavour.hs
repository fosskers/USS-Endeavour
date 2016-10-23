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
import           Endeavour.Knowledge.LittleBits
import           Endeavour.Memory
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Servant.API
import           Servant.Server

---

-- | Command-line arguments.
data Args = Args { config :: FilePath } deriving (Generic)

instance ParseRecord Args

type API = "lamp" :> Get '[JSON] ()

api :: Proxy API
api = Proxy

-- | The request handler functions, all of which operate in the `Effect` Monad.
serverT :: ServerT API Effect
serverT = lamp

effToHandler' :: Env -> Effect a -> Handler a
effToHandler' env eff = liftIO (runLift . runExc $ runReader eff env) >>= either f pure
  where f err = liftIO (chronicle' (_conn env) Fail err) >> throwE err404

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
      wake $ _conn e
      W.run 8081 $ app e  -- How do we ever exit this?
      slumber e           -- Catch exceptions, as well as C-c or C-d. How?
