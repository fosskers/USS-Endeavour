{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Knowledge.Space
-- Copyright : (c) Colin Woodbury, 2016 - 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- How many human beings are in space?

module Endeavour.Knowledge.Space
  ( Astronaut(..)
  , astronauts
  ) where

import           Data.Aeson
import           Data.Proxy
import qualified Data.Text as T
import           Endeavour.Genetics
import           Endeavour.Knowledge.Util
import           GHC.Generics
import           Servant.API
import           Servant.Client

---

data Astronaut = Astronaut { name :: T.Text, craft :: T.Text } deriving (Generic, FromJSON)

newtype InSpace = InSpace { _astros :: [Astronaut] }

instance FromJSON InSpace where
  parseJSON (Object o) = InSpace <$> o .: "people"

type SpaceAPI = "astros.json" :> Get '[JSON] InSpace

api :: Proxy SpaceAPI
api = Proxy

url :: BaseUrl
url = BaseUrl Http "api.open-notify.org" 80 ""

inSpace :: ClientM InSpace
inSpace = client api

-- | Humans currently in space.
astronauts :: ERL r => Eff r [Astronaut]
astronauts = _astros <$> transmit url inSpace
