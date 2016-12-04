{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Endeavour.Knowledge.Hue
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Control the state of Phillips Hue lights on a home network.

module Endeavour.Knowledge.Hue
  ( -- * Lights
    Light(..)
    -- ** Light Status
  , light
  , lights
    -- ** Light Controls
  , allOn, allOff
  , lightOn, lightOff
  , lightBri, lightBri'
  , lightSat, lightSat'
  , lightHue, lightHue'
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
lightOn :: ERL r => Int -> Eff r ()
lightOn = overLight f
  where f l@Light { _on = Left False } = l { _on = Right True }
        f l = l

-- | Turn a light off.
lightOff :: ERL r => Int -> Eff r ()
lightOff = overLight f
  where f l@Light { _on = Left True } = l { _on = Right False }
        f l = l

-- | Set the brightness, as a percent of its maximum (254).
-- Pass the `Float` in as a value between 0 and 1.
lightBri :: ERL r => Float -> Int -> Eff r ()
lightBri p = overLight (& bri .~ round (254 * p))

-- | Set the brightness, as a percent of its current value.
-- Pass the `Float` in as a value between 0 and 1.
lightBri' :: ERL r => Float -> Int -> Eff r ()
lightBri' p = overLight (& bri %~ (\n -> round $ fromIntegral n * p))

-- | Set a light's `Colour`.
lightHue :: ERL r => Colour -> Int -> Eff r ()
lightHue = lightHue' . fromJust . flip M.lookup colours

-- | Set a light's hue value.
lightHue' :: ERL r => Word16 -> Int -> Eff r ()
lightHue' h = overLight (& hue .~ h)

-- | Set the saturation, as a percent of its maximum (254).
lightSat :: ERL r => Float -> Int -> Eff r ()
lightSat p = overLight (& sat .~ round (254 * p))

-- | Set the saturation, as a percent of its current value.
lightSat' :: ERL r => Float -> Int -> Eff r ()
lightSat' p = overLight (& sat %~ (\n -> round $ fromIntegral n * p))

lightEffect :: ERL r => LightEffect -> Int -> Eff r ()
lightEffect e = overLight (& effect .~ e)

groups :: ERL r => Eff r [Group]
groups = undefined

-- | Turn all the lights on.
allOn :: ERL r => Eff r ()
allOn = lights >>= mapM_ lightOn . M.keys

-- | Turn all the lights off.
allOff :: ERL r => Eff r ()
allOff = lights >>= mapM_ lightOff . M.keys

groupOn :: ERL r => Group -> Eff r ()
groupOn = undefined

groupOff :: ERL r => Group -> Eff r ()
groupOff = undefined
