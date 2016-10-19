{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Endeavour.Knowledge.LittleBits.Internal
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- LittleBits Cloud Control API query logic.

module Endeavour.Knowledge.LittleBits.Internal
  ( CBStatus(..)
  , status
  , transmit
  ) where

import Control.Eff hiding ((:>))
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Data.Aeson
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Endeavour.Genetics
import Servant.API
import Servant.Client

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
data CBStatus = CBStatus { _label :: Text
                         , _id :: Text
                         , _userId :: Int
                         , _isConnected :: Bool } deriving (Show, Eq)

instance FromJSON CBStatus where
  parseJSON (Object v) = CBStatus       <$>
                         v .: "label"   <*>
                         v .: "id"      <*>
                         v .: "user_id" <*>
                         v .: "is_connected"

type LBCCApi = "devices" :> Capture "device_id" String :> Header "Authorization" String :> Get '[JSON] CBStatus

api :: Proxy LBCCApi
api = Proxy

_device = client api

-- | The current status of the CloudBit.
status :: ERL r => Eff r CBStatus
status = do
  (CloudBit did auth) <- reader _cloudbit
  transmit . _device (unpack did) . Just $ "Bearer " ++ unpack auth

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api-http.littlebitscloud.cc" 443 ""

-- | Make some call to an external API.
transmit :: ERL r => ClientM a -> Eff r a
transmit m = do
  manager <- reader _manager
  res <- lift $ runClientM m (ClientEnv manager baseUrl)
  case res of
    Left err -> throwExc . pack $ show err
    Right r  -> pure r
