{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Knowledge.LittleBits
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- LittleBits Cloud Control API query logic.

module Endeavour.Knowledge.LittleBits where

import Control.Eff
import Endeavour.Genetics
import Endeavour.Knowledge.LittleBits.Internal
import Endeavour.Memory

---

-- | Turn the lamp on and off.
lamp :: ERL r => Eff r ()
lamp = emit (CBOutput 100 1500) >> chronicle Info "Lamp activated."