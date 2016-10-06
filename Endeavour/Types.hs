{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Endeavour.Types where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Database.SQLite.Simple

---

type RIO r = (Member (Reader Env) r, SetMember Lift (Lift IO) r)

data Env = Env { _conn :: Connection }
