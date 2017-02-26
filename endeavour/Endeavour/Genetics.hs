{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Endeavour.Genetics
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Data types and aliases used across the code base.

module Endeavour.Genetics where

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Data.Aeson (Value)
import qualified Data.Text as T
import           Data.Void
import           Data.Yaml (decodeFile)
import           Database.SQLite.Simple
import           Lens.Micro
import           Lens.Micro.Aeson
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS

---

-- | Functions who need the @Lift IO@ effect.
type L r = SetMember Lift (Lift IO) r

-- | Functions who need the `Reader` and @Lift IO@ effects.
type RL r = (Member (Reader Env) r, L r)

-- | Functions who need the `Exc`, `Reader`, and @Lift IO@ effects.
type ERL r = (Member (Exc T.Text) r, RL r)

-- | The full effect stack, ordered strategically for interplay with `servant`.
type Effect = Eff (Reader Env :> Exc T.Text :> Lift IO :> Void)

-- | A <http://littlebits.cc/ LittleBits> CloudBit's device ID and auth token.
data CloudBit = CloudBit { _deviceId :: T.Text
                         , _authTok  :: T.Text } deriving (Show)

-- | The Endeavour's runtime environment.
data Env = Env { _conn     :: Connection
               , _cloudbit :: CloudBit
               , _manager  :: Manager
               , _media    :: T.Text
               , _hueUser  :: T.Text
               , _hueIp    :: T.Text }

-- | Run the full `Effect` stack. Useful for bringing backend actions
-- into the `IO` monad, for further lifting via `liftIO`.
runEffect :: Env -> Effect a -> IO (Either T.Text a)
runEffect env eff = runLift . runExc $ runReader eff env

-- | Given a `FilePath` to a config file, read it and parse out the runtime
-- environment.
awaken :: FilePath -> IO (Maybe Env)
awaken conf = do
  raw <- decodeFile @Value conf
  let db   = raw ^? _Just . key "db_path"    . _String
      iden = raw ^? _Just . key "cloud_bit"  . key "device_id"  . _String
      auth = raw ^? _Just . key "cloud_bit"  . key "auth_token" . _String
      meda = raw ^? _Just . key "media_path" . _String
      husr = raw ^? _Just . key "hue"        . key "user"       . _String
      huip = raw ^? _Just . key "hue"        . key "ip"         . _String
  sequence $ f <$> db <*> iden <*> auth <*> meda <*> husr <*> huip -- clever
  where f d i a m hu hi = do
          conn <- open $ T.unpack d
          manager <- newManager tlsManagerSettings
          pure $ Env conn (CloudBit i a) manager m hu hi

-- | Shutdown any session handling in a given `Env`.
slumber :: Env -> IO ()
slumber = close . _conn
