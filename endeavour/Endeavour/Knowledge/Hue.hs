{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Endeavour.Knowledge.Hue where

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
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import Servant.API
import Servant.Client

---

{-

Can apply changes to a single light, or a Group.

Need: `foo :: Group -> [Light]`

Then you map the change across each light simultaneously.
Or no... since the bridge allows for group-based calls. You'd only have to
make one HTTP request to change all the lights.

Which Hue value (0 - 65535) corresponds to what colour?

Is it the most elegant to modify the Light state with Lenses, and send a
JSON object with the entire new state over?

-}

-- | A reduced representation of a Phillips Hue light.
data Light = Light { _lname :: Text
                   , _on    :: Bool
                   , _state :: LState } deriving (Eq, Show)

instance FromJSON Light where
  parseJSON (Object v) = Light   <$>
    v  .: "name"                 <*>
    (v .: "state" >>= (.: "on")) <*>
    v  .: "state"

-- | Special effects that a light can perform.
data LightEffect = NoEffect | ColourLoop | Flash deriving (Eq, Show)

instance ToJSON LightEffect where
  toJSON ColourLoop = "colorloop"
  toJSON _ = "none"

instance FromJSON LightEffect where
  parseJSON (String "colorloop") = pure ColourLoop
  parseJSON _ = pure NoEffect

-- | The state of a `Light`. Note that "on-ness" isn't recorded here,  since
-- `LState`s are meant to be serialized and sent frequently to the Bridge.
-- According to Phillips, constantly resending @{"on":true}@ when a light is
-- already on slows the responsiveness of the Bridge.
data LState = LState { bri    :: Word8
                     , hue    :: Word16
                     , sat    :: Word8
                     , effect :: LightEffect } deriving (Eq, Show, Generic)
makeLensesFor [("bri", "briL"), ("hue", "hueL"), ("sat", "satL")] ''LState

instance ToJSON LState
instance FromJSON LState

foo :: LState
foo = LState 254 20000 254 NoEffect

data Group = Group { _gid    :: Int
                   , _gname  :: Text
                   , _lights :: [Light]
                   , _allOn  :: Bool
                   , _anyOn  :: Bool
                   , _action :: LState } deriving (Eq, Show)

data Colour = Red | Yellow | Green | Blue | Magenta deriving (Eq, Show, Ord, Enum)

type HueApi = "api" :> Capture "uid" Text :> LApi

type LApi = "lights" :> Get '[JSON] (Map Text Light)
  :<|> "lights" :> Capture "lid" Int :> Get '[JSON] Light
  :<|> "lights" :> Capture "lid" Int :> "state" :> ReqBody '[JSON] LState :> Put '[JSON] NoContent

api :: Proxy HueApi
api = Proxy

handlers :: Text
  -> ClientM (Map Text Light)
  :<|> (Int -> ClientM Light)
  :<|> (Int -> LState -> ClientM NoContent)
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

bri2 :: ERL r => Word8 -> Eff r ()
bri2 b = do
  hu <- reader _hueUser
  hi <- reader _hueIp
  let (_ :<|> f :<|> g) = handlers hu
  ls <- fmap _state . transmit (bridgeUrl hi) $ f 2
  void . transmit (bridgeUrl hi) . g 2 $ (ls & briL .~ b)

hue2 :: ERL r => Word16 -> Eff r ()
hue2 b = do
  hu <- reader _hueUser
  hi <- reader _hueIp
  let (_ :<|> f :<|> g) = handlers hu
  ls <- fmap _state . transmit (bridgeUrl hi) $ f 2
  void . transmit (bridgeUrl hi) . g 2 $ (ls & hueL .~ b)

groups :: ERL r => Eff r [Group]
groups = undefined

-- | Turn all the lights on.
allOn :: ERL r => Eff r ()
allOn = undefined

-- | Turn all the lights off.
allOff :: ERL r => Eff r ()
allOff = undefined

groupOn :: ERL r => Group -> Eff r ()
groupOn = undefined

groupOff :: ERL r => Group -> Eff r ()
groupOff = undefined

lightOn :: ERL r => Light -> Eff r ()
lightOn = undefined

lightOff :: ERL r => Light -> Eff r ()
lightOff = undefined

-- | Set the brightness, as a percent of its maximum (254).
lightBri :: ERL r => Int -> Light -> Eff r ()
lightBri = undefined
