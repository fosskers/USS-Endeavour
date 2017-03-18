{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Endeavour.Knowledge.LittleBits.Internal
-- Copyright : (c) Colin Woodbury, 2016 - 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- LittleBits Cloud Control API query logic.

module Endeavour.Knowledge.LittleBits.Internal
  ( CBStatus(..)
  , CBOutput(..)
  , status
  , emit
  ) where

import Control.Monad (void)
import Data.Aeson
import Data.Proxy
import Data.Text (Text, unpack)
import Endeavour.Genetics
import Endeavour.Knowledge.Util
import Servant.API
import Servant.Client

---

-- | A CloudBit's current status. Stores less than is actually returned by
-- the LittleBits Cloud Control.
data CBStatus = CBStatus { _label :: Text
                         , _id :: Text
                         , _userId :: Int
                         , _isConnected :: Bool } deriving (Show, Eq)

instance ToJSON CBStatus where
  toJSON (CBStatus l i u c) = object [ "label" .= l
                                     , "id" .= i
                                     , "user_id" .= u
                                     , "is_connected" .= c ]

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
  (CloudBit did auth) <- asks _cloudbit
  void . transmit baseUrl $ _output (unpack did) (header auth) cbo

-- | The current status of the CloudBit.
status :: ERL r => Eff r CBStatus
status = do
  (CloudBit did auth) <- asks _cloudbit
  transmit baseUrl . _device (unpack did) $ header auth

-- | Format a header for the CloudBit auth token.
header :: Text -> Maybe String
header (unpack -> auth) = Just $ "Bearer " ++ auth

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api-http.littlebitscloud.cc" 443 ""
