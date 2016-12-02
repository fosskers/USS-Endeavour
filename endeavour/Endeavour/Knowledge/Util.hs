{-# LANGUAGE FlexibleContexts #-}

module Endeavour.Knowledge.Util where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Data.Text (pack)
import Endeavour.Genetics
import Servant.Client

---

-- | Make some call to an external API via @servant-client@.
transmit :: ERL r => BaseUrl -> ClientM a -> Eff r a
transmit u m = do
  manager <- reader _manager
  res <- lift $ runClientM m (ClientEnv manager u)
  case res of
    Left err -> throwExc . pack $ show err
    Right r  -> pure r
