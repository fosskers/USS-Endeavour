{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Endeavour.Knowledge.LittleBits.Internal
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- LittleBits Cloud Control API query logic.

module Endeavour.Knowledge.LittleBits.Internal
  ( CBStatus(..)
  , CBOutput(..)
  , status
  , emit
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

-- | Output parameters to send to a CloudBit.
data CBOutput = CBOutput { _percent  :: Int,  -- ^ 0 to 100.
                           _duration :: Int   -- ^ In milliseconds.
                         }

instance ToJSON CBOutput where
  toJSON (CBOutput p d) = object ["percent" .= p, "duration_ms" .= d]

type LBCCApi = "v2"
             :> "devices"
             :> Capture "device_id" String
             :> Header "Authorization" String
             :> Get '[JSON] CBStatus
           :<|> "v2"
             :> "devices"
             :> Capture "device_id" String
             :> "output"
             :> Header "Authorization" String
             :> ReqBody '[JSON] CBOutput
             :> PostNoContent '[JSON] NoContent

api :: Proxy LBCCApi
api = Proxy

_device :<|> _output = client api

-- | Cause a CloudBit to emit voltage.
emit :: ERL r => CBOutput -> Eff r ()
emit cbo = do
  (CloudBit did auth) <- reader _cloudbit
  fmap (const ()) . transmit . _output (unpack did) (header auth) $ cbo

-- | The current status of the CloudBit.
status :: ERL r => Eff r CBStatus
status = do
  (CloudBit did auth) <- reader _cloudbit
  transmit . _device (unpack did) $ header auth

-- | Format a header for the CloudBit auth token.
header :: Text -> Maybe String
header (unpack -> auth) = Just $ "Bearer " ++ auth

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