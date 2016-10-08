{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Knowledge.LittleBits.Internal
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- LittleBits Cloud Control API query logic.

module Endeavour.Knowledge.LittleBits.Internal where

import Endeavour.Types
import Control.Eff hiding ((:>))
import Control.Eff.Reader.Lazy
import Control.Eff.Lift
import Control.Eff.Exception
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client
import Data.Text (Text, pack)

---

{- SAMPLE RESPONSE

https://api-http.littlebitscloud.cc/devices/00e04c03b6d6

{
 "label": "Computer",
 "id": "00e04c03b6d6",
 "subscriptions": [],
 "subscribers": [
  {
   "publisher_id": "00e04c03b6d6",
   "subscriber_id": "https:\/\/api-ifttt.littlebitscloud.cc\/receive_cloudbit_event\/ignite\/e2520f84f530dcd3305ff3e255f1f9e3f1cf2e6f",
   "publisher_events": [
    "amplitude:delta:ignite"
   ]
  }
 ],
 "user_id": 135545,
 "is_connected": true,
 "input_interval_ms": 200
}

-}

-- | A CloudBit's current status. Stores less than is actually returned by
-- the LittleBits Cloud Control.
data Status = Status { _label :: Text
                     , _id :: Text
                     , _userId :: Int
                     , _isConnected :: Bool
                     } deriving (Generic, Show)

instance FromJSON Status

type LBCCApi = "devices" :> Get '[JSON] Status

api :: Proxy LBCCApi
api = Proxy

devices :: ClientM Status
devices = client api

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api-http.littlebitscloud.cc" 443 ""

-- | Make some call to an external API.
transmit :: ERIO r => ClientM a -> Eff r a
transmit m = do
  manager <- reader _manager
  res <- lift $ runClientM m (ClientEnv manager baseUrl)
  case res of
    Left err -> throwExc . pack $ show err
    Right r  -> pure r
