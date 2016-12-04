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
    -- * Functions
  , light
  , lights
  , overLight
    -- * Lenses
  , on, bri, hue, sat, effect
  ) where

import Control.Eff hiding ((:>))
import Control.Eff.Reader.Lazy
import Control.Monad (void)
import Data.Aeson
import Data.Map.Lazy (Map, mapKeys)
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

data Group = Group { _gname  :: Text
                   , _lights :: [Int]
                   , _action :: Light } deriving (Eq, Show)

instance FromJSON Group where
  parseJSON (Object v) = Group       <$>
    v .: "name"                      <*>
    ((map read) <$> (v .: "lights")) <*>
    v .: "action"

type API =
  "api" :> Capture "uid" Text :> "lights" :> Get '[JSON] (Map Text Light)
  :<|> "api" :> Capture "uid" Text :> "lights" :> Capture "lid" Int :> Get '[JSON] Light
  :<|> "api" :> Capture "uid" Text :> "lights" :> Capture "lid" Int :> "state" :> ReqBody '[JSON] Light :> Put '[JSON] NoContent
  :<|> "api" :> Capture "uid" Text :> "groups" :> Get '[JSON] (Map Text Group)
  :<|> "api" :> Capture "uid" Text :> "groups" :> Capture "gid" Int :> Get '[JSON] Group
  :<|> "api" :> Capture "uid" Text :> "groups" :> Capture "gid" Int :> "action" :> ReqBody '[JSON] Light :> Put '[JSON] NoContent

api :: Proxy API
api = Proxy

-- TODO Can these functions, surrounded by parens, take a parameter?
-- `(foo :<|> bar) text = client api`
-- And if so, what does that mean for functionality? How are they called?
-- | Pattern match our way to our handler functions.
getLights :<|> getLight :<|> setLight :<|> getGroups :<|> getGroup :<|> setGroup = client api

-- | The URL (likely an IP address) of the Hue Bridge on the home network.
-- This IP must be set in the external YAML config.
bridgeUrl :: Text -> BaseUrl
bridgeUrl (unpack -> u) = BaseUrl Http u 80 ""

-- | Make a call to the Bridge.
toBridge :: ERL r => (Text -> ClientM a) -> Eff r a
toBridge c = do
  hu <- reader _hueUser
  hi <- reader _hueIp
  transmit (bridgeUrl hi) $ c hu

-- | A `Light`, from its ID.
light :: ERL r => Int -> Eff r Light
light lid = toBridge $ flip getLight lid

-- | All the lights in the network.
lights :: ERL r => Eff r (Map Int Light)
lights = toBridge getLights >>= pure . mapKeys (read . unpack)

-- | A function over a light, across a network.
overLight :: ERL r => (Light -> Light) -> Int -> Eff r ()
overLight f lid = light lid >>= void . toBridge . (\l t -> setLight t lid l) . f

{-}
group :: ERL r => Int -> Eff r Group
group gid = do
  hu <- reader _hueUser
  let (_ :<|> f :<|> _) = gHandlers hu
  toBridge $ f gid

groups :: ERL r => Eff r (Map Int Group)
groups = do
  hu <- reader _hueUser
  let (f :<|> _) = gHandlers hu
  mapKeys (read . unpack) <$> toBridge f
-}
