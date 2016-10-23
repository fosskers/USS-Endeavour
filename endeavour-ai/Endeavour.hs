{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Concurrent
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import qualified Control.Exception as E
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
import           System.Exit
import           System.Posix.Signals hiding (Handler)

---

-- | Command-line arguments.
data Args = Args { config :: FilePath } deriving (Generic)

instance ParseRecord Args

type API = "lamp" :> Get '[JSON] ()
  :<|> "log" :> QueryParam "limit" Int :> Get '[JSON] [Log]

api :: Proxy API
api = Proxy

-- | The request handler functions, all of which operate in the `Effect` Monad.
serverT :: ServerT API Effect
serverT = lamp :<|> recall

-- | Conversion logic between our effect stack and the `Handler` Monad.
-- Catches any errors thrown within the effect stack, writes them to the
-- ship's DB, and rethrows them into the `Handler` Monad. `Handler` is
-- just a type alias for `ExceptT`.
effToHandler' :: Env -> Effect a -> Handler a
effToHandler' env eff = liftIO (runLift . runExc $ runReader eff env) >>= either f pure
  where f err = liftIO (chronicle' (_conn env) Fail err) >> throwE err404

-- | A Natural Transformation between our effect stack and the `Handler` Monad.
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
      tid <- myThreadId
      let h = slumber e >> putStrLn "Shutting down." >> E.throwTo tid ExitSuccess
      installHandler keyboardSignal (Catch h) Nothing
      W.run 8081 $ app e
