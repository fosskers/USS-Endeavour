{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Types
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Data types and aliases used across the code base.

module Endeavour.Types where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Data.Text (Text)
import Database.SQLite.Simple
import Network.HTTP.Client (Manager)

---

-- | Functions who need the @Lift IO@ effect.
type L r = SetMember Lift (Lift IO) r

-- | Functions who need the `Reader` and @Lift IO@ effects.
type RL r = (Member (Reader Env) r, L r)

-- | Functions who need the `Exc`, `Reader`, and @Lift IO@ effects.
type ERL r = (Member (Exc Text) r, RL r)

-- | A <http://littlebits.cc/ LittleBits> CloudBit's device ID and auth token.
data CloudBit = CloudBit Text Text

-- | The Endeavour's runtime environment.
data Env = Env { _conn :: Connection
               , _cloudbit :: CloudBit
               , _manager :: Manager
               }
