{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Void
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
                         , _auth_tok :: T.Text } deriving (Show)

-- | The Endeavour's runtime environment.
data Env = Env { _conn     :: Connection
               , _cloudbit :: CloudBit
               , _manager  :: Manager }

-- | Given a `FilePath` to a config file, read it and parse out the runtime
-- environment.
awaken :: FilePath -> IO (Maybe Env)
awaken conf = do
  raw <- TIO.readFile conf
  let db   = raw ^? key "db_path" . _String
      iden = raw ^? key "cloud_bit" . key "device_id" . _String
      auth = raw ^? key "cloud_bit" . key "auth_token" . _String
  sequence $ f <$> db <*> iden <*> auth  -- clever
  where f d i a = do
          conn <- open $ T.unpack d
          manager <- newManager tlsManagerSettings
          pure $ Env conn (CloudBit i a) manager

-- | Shutdown any session handling in a given `Env`.
slumber :: Env -> IO ()
slumber = close . _conn
