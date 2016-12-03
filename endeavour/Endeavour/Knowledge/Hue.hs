{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

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

-- | Only encode the @on@ field if there was a change. This change is
-- reflected in the transformation from `Left` to `Right`. Not passing @on@
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

-- | Turn a light on.
lightOn :: ERL r => Int -> Eff r ()
lightOn = overLight f
  where f l@Light { _on = Left False } = l { _on = Right True }
        f l = l

-- | Turn a light off.
lightOff :: ERL r => Int -> Eff r ()
lightOff = overLight f
  where f l@Light { _on = Left True } = l { _on = Right False }
        f l = l

-- | Set a light's brightness.
lightBri :: ERL r => Word8 -> Int -> Eff r ()
lightBri b = overLight (& bri .~ b)

-- | Set the brightness, as a percent of its maximum (254).
lightBri' :: ERL r => Float -> Int -> Eff r ()
lightBri' p = overLight (& bri .~ round (254 * p))

-- | Set the brightness, as a percent of its current value.
lightBri'' :: ERL r => Float -> Int -> Eff r ()
lightBri'' p = overLight (& bri %~ (\n -> round $ fromIntegral n * p))

-- | Set a light's hue.
lightHue :: ERL r => Word16 -> Int -> Eff r ()
lightHue h = overLight (& hue .~ h)

-- | Set the saturation, as a percent of its maximum (254).

lightSat :: ERL r => Word8 -> Int -> Eff r ()
lightSat s = overLight (& sat .~ s)

lightEffect :: ERL r => LightEffect -> Int -> Eff r ()
lightEffect e = overLight (& effect .~ e)

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
