{-# LANGUAGE FlexibleContexts #-}

module Endeavour.Knowledge.Util where

import Data.Text (pack)
import Endeavour.Genetics
import Servant.Client

---

-- | Make some call to an external API via @servant-client@.
transmit :: ERL r => BaseUrl -> ClientM a -> Eff r a
transmit u m = do
  manager <- asks _manager
  res <- send $ runClientM m (ClientEnv manager u)
  case res of
    Left err -> throwError . pack $ show err
    Right r  -> pure r
