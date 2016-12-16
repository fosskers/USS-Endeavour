{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Knowledge.Hue
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Control the state of Phillips Hue lights on a home network.

module Endeavour.Knowledge.Hue
  ( -- * Lights
    -- ** Types
    Light(..)
  , LightEffect(..)
  , Group(..)
  , ID(..)
    -- ** Light Status
  , light, lights
  , group, groups
    -- ** Light Controls
    -- | The core transformations that affect `Light`s are all pure. To "run"
    -- a transformation, use the `overLight` function:
    --
    -- > turnItOn :: ERL r => ID -> Eff r ()
    -- > turnItOn = overLight lightOn
    --
    -- This will apply the change across the network to the `Light` whose
    -- id you pass in as the `ID`. Efficiently applying multiple transformations
    -- to a single `Light` is also easy:
    --
    -- > dimBlue :: ERL r => ID -> Eff r ()
    -- > dimBlue = overLight (lightHue Blue . lightBri 0.1)
    --
    -- To run transformations across light `Group`s, use `overGroup`:
    --
    -- > makeGroupRed :: ERL r => ID -> Eff r ()
    -- > makeGroupRed = overGroup (lightHue Red)
  , overLight, overGroup
  , lightOn, lightOff
  , lightBri, lightBri'
  , lightSat, lightSat'
  , lightHue, lightHue'
  , lightEffect
  , allOn, allOff
    -- * Colours
  , Colour(..)
  , colours
  ) where

import           Control.Eff
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import           Data.Word
import           Endeavour.Genetics
import           Endeavour.Knowledge.Hue.Internal
import           Lens.Micro

---

data Colour = Red | Yellow | Green | Blue | Magenta deriving (Eq, Show, Ord, Enum)

-- | A correspondance between human identifiable colours and their integer
-- hue value that the bulbs use.
colours :: M.Map Colour Word16
colours = M.fromList $ zip [Red ..] [ 0, 12750, 25500, 46920, 56100 ]

-- | Turn a light on.
lightOn :: Light -> Light
lightOn l@Light { _on = Left False } = l { _on = Right True }
lightOn l = l

-- | Turn a light off.
lightOff :: Light -> Light
lightOff l@Light { _on = Left True } = l { _on = Right False }
lightOff l = l

-- | Set the brightness, as a percent of its maximum (254).
-- Pass the `Float` in as a value between 0 and 1.
lightBri :: Float -> Light -> Light
lightBri p l = l & bri .~ round (254 * p)

-- | Set the brightness, as a percent of its current value.
-- Pass the `Float` in as a value between 0 and 1.
lightBri' :: Float -> Light -> Light
lightBri' p l = l & bri %~ (\n -> round $ fromIntegral n * p)

-- | Set a light's `Colour`.
lightHue :: Colour -> Light -> Light
lightHue = lightHue' . fromJust . flip M.lookup colours

-- | Set a light's hue value.
lightHue' :: Word16 -> Light -> Light
lightHue' h l = l & hue .~ h

-- | Set the saturation, as a percent of its maximum (254).
lightSat :: Float -> Light -> Light
lightSat p l = l & sat .~ round (254 * p)

-- | Set the saturation, as a percent of its current value.
lightSat' :: Float -> Light -> Light
lightSat' p l = l & sat %~ (\n -> round $ fromIntegral n * p)

lightEffect :: LightEffect -> Light -> Light
lightEffect e l = l & effect .~ e

-- | Turn all the lights on.
allOn :: ERL r => Eff r ()
allOn = lights >>= mapM_ (overLight lightOn) . M.keys

-- | Turn all the lights off.
allOff :: ERL r => Eff r ()
allOff = lights >>= mapM_ (overLight lightOff) . M.keys
