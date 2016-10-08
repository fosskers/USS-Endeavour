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

-- | To simplify signatures of functions who need the `Reader` and @Lift IO@ effects.
type RIO r = (Member (Reader Env) r, SetMember Lift (Lift IO) r)

-- | Functions who need the `Reader`, `Exc`, and @Lift IO@ effects.
type ERIO r = (Member (Reader Env) r, SetMember Lift (Lift IO) r, Member (Exc Text) r)

-- | A <http://littlebits.cc/ LittleBits> CloudBit's device ID and auth token.
data CloudBit = CloudBit Text Text

-- | The Endeavour's runtime environment.
data Env = Env { _conn :: Connection
               , _cloudbit :: CloudBit
               , _manager :: Manager
               }
