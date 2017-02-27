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
  , ID(..)
    -- * Functions
  , light, lights, overLight
  , group, groups, overGroup
    -- * Lenses
  , on, bri, hue, sat, effect
  , gname, glights, gaction
  ) where

import Control.Eff hiding ((:>))
import Control.Eff.Reader.Lazy
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (Parser)
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

-- | A typesafe Integer id for a Phillips Hue lights and light groups.
newtype ID = ID { _id :: Word8 } deriving (Eq, Ord, Show)

instance FromJSON ID where
  parseJSON (String n) = pure . ID . read $ unpack n

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
  parseJSON (Object v) = (v .: "state") >>= lightP

-- | A field parser common to `Light` and `Group`.
lightP :: Object -> Parser Light
lightP s = Light
  <$> fmap Left (s .: "on")
  <*> s .: "bri"
  <*> s .: "hue"
  <*> s .: "sat"
  <*> s .: "effect"

-- | Only encode the @"on"@ field if there was a change. This change is
-- reflected in the transformation from `Left` to `Right`. Not passing @"on"@
-- when you don't need to reduces load on the Bridge.
instance ToJSON Light where
  toJSON (Light o b h s e) = object $ o' o ++ [ "bri" .= b, "hue" .= h, "sat" .= s, "effect" .= e ]
    where o' (Right b) = [ "on" .= b ]  -- A change occurred.
          o' _ = []

data Group = Group { _gname  :: Text
                   , _glights :: [ID]
                   , _gaction :: Light } deriving (Eq, Show)
makeLenses ''Group

instance FromJSON Group where
  parseJSON (Object v) = Group <$> v .: "name" <*> v .: "lights" <*> (v .: "action" >>= lightP)

type API =
  "api" :> Capture "uid" Text :> "lights" :> Get '[JSON] (Map Text Light)
  :<|> "api" :> Capture "uid" Text :> "lights" :> Capture "lid" Word8 :> Get '[JSON] Light
  :<|> "api" :> Capture "uid" Text :> "lights" :> Capture "lid" Word8 :> "state" :> ReqBody '[JSON] Light :> Put '[JSON] NoContent
  :<|> "api" :> Capture "uid" Text :> "groups" :> Get '[JSON] (Map Text Group)
  :<|> "api" :> Capture "uid" Text :> "groups" :> Capture "gid" Word8 :> Get '[JSON] Group

api :: Proxy API
api = Proxy

-- | Pattern match our way to our handler functions.
getLights :<|> getLight :<|> setLight :<|> getGroups :<|> getGroup = client api

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

-- | A `Light`, from its `ID`.
light :: ERL r => ID -> Eff r Light
light = toBridge . flip getLight . _id

-- | All the lights in the network.
lights :: ERL r => Eff r (Map ID Light)
lights = mapKeys (ID . read . unpack) <$> toBridge getLights

-- | A function over a light, across a network.
overLight :: ERL r => (Light -> Light) -> ID -> Eff r ()
overLight f lid = light lid >>= void . toBridge . (\l t -> setLight t (_id lid) l) . f

-- | A `Group`, from its `ID`.
group :: ERL r => ID -> Eff r Group
group = toBridge . flip getGroup . _id

-- | All the groups in the network.
groups :: ERL r => Eff r (Map ID Group)
groups = mapKeys (ID . read . unpack) <$> toBridge getGroups

-- | Given a function which transforms a `Light`, perform
-- that action on all lights in a `Group`.
overGroup :: ERL r => (Light -> Light) -> ID -> Eff r ()
overGroup f g = group g >>= mapM_ (overLight f) . _glights
