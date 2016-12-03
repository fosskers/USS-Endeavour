{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Endeavour.Knowledge.Hue.Internal
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Inner logic and types for connecting to Phillips Hue lights.

module Endeavour.Knowledge.Hue.Internal
  ( -- * Types
    Light(..)
  , LightEffect(..)
  , Group(..)
  , Colour(..)
    -- * Functions
  , colours
  , lights
  , overLight
    -- * Lenses
  , on, bri, hue, sat, effect
  ) where

import Control.Eff hiding ((:>))
import Control.Eff.Reader.Lazy
import Control.Monad (void)
import Data.Aeson
import Data.Map.Lazy (Map, fromList)
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Word
import Endeavour.Genetics
import Endeavour.Knowledge.Util
import Lens.Micro.TH
import Servant.API
import Servant.Client

---

-- | Special effects that a light can perform.
data LightEffect = NoEffect | ColourLoop | Flash deriving (Eq, Show)

instance ToJSON LightEffect where
  toJSON ColourLoop = "colorloop"
  toJSON _ = "none"

instance FromJSON LightEffect where
  parseJSON (String "colorloop") = pure ColourLoop
  parseJSON _ = pure NoEffect

-- | A simplified representation of a Phillips Hue light.
data Light = Light { _on     :: Either Bool Bool
                   , _bri    :: Word8
                   , _hue    :: Word16
                   , _sat    :: Word8
                   , _effect :: LightEffect } deriving (Eq, Show)
makeLenses ''Light

instance FromJSON Light where
  parseJSON (Object v) = (v .: "state") >>= \s ->
    Light                      <$>
    fmap Left (s .: "on")      <*>
    s .: "bri"                 <*>
    s .: "hue"                 <*>
    s .: "sat"                 <*>
    s .: "effect"

-- | Only encode the @"on"@ field if there was a change. This change is
-- reflected in the transformation from `Left` to `Right`. Not passing @"on"@
-- when you don't need to reduces load on the Bridge.
instance ToJSON Light where
  toJSON (Light o b h s e) = object $ o' o ++ [ "bri" .= b, "hue" .= h, "sat" .= s, "effect" .= e ]
    where o' (Right b) = [ "on" .= b ]  -- A change occurred.
          o' _ = []

data Group = Group { _gid    :: Int
                   , _gname  :: Text
                   , _lights :: [Light]
                   , _allOn  :: Bool
                   , _anyOn  :: Bool
                   , _action :: Light } deriving (Eq, Show)

data Colour = Red | Yellow | Green | Blue | Magenta deriving (Eq, Show, Ord, Enum)

type HueApi = "api" :> Capture "uid" Text :> LApi

type LApi = "lights" :> Get '[JSON] (Map Text Light)
  :<|> "lights" :> Capture "lid" Int :> Get '[JSON] Light
  :<|> "lights" :> Capture "lid" Int :> "state" :> ReqBody '[JSON] Light :> Put '[JSON] NoContent

api :: Proxy HueApi
api = Proxy

handlers :: Text
  -> ClientM (Map Text Light)
  :<|> (Int -> ClientM Light)
  :<|> (Int -> Light -> ClientM NoContent)
handlers = client api

-- | The URL (likely an IP address) of the Hue Bridge on the home network.
-- This IP must be set in the external YAML config.
bridgeUrl :: Text -> BaseUrl
bridgeUrl (unpack -> u) = BaseUrl Http u 80 ""

-- | A correspondance between human identifiable colours and their integer
-- hue value that the bulbs use.
colours :: Map Colour Int
colours = fromList $ zip [Red ..] [ 0, 12750, 25500, 46920, 56100 ]

-- | All the lights in the network.
lights :: ERL r => Eff r (Map Text Light)
lights = do
  hu <- reader _hueUser
  hi <- reader _hueIp
  let (f :<|> _) = handlers hu
  transmit (bridgeUrl hi) f

-- | A function over a light, across a network.
overLight :: ERL r => (Light -> Light) -> Int -> Eff r ()
overLight f lid = do
  hu <- reader _hueUser
  hi <- reader _hueIp
  let (_ :<|> g :<|> h) = handlers hu
  l <- transmit (bridgeUrl hi) $ g lid
  void . transmit (bridgeUrl hi) . h lid $ f l
