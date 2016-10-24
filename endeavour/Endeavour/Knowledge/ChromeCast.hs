{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Endeavour.Knowledge.ChromeCast
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Functions for interacting with a ChromeCast.

module Endeavour.Knowledge.ChromeCast
  ( cast
  , cast'
  ) where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Data.Monoid
import Data.Text (Text)
import Endeavour.Genetics
import Endeavour.Memory
import Shelly

---

cast :: ERL r => Maybe Text -> Eff r ()
cast Nothing = throwExc ("No file given to stream." :: Text)
cast (Just f) = cast' f

-- | ChromeCast can handle MP4, MKV, and normal audio formats (including FLAC)
-- natively. AVIs will be converted on-the-fly via the @--tomp4@ flag.
--
-- @castnow@ will exit successfully when the file is done playing,
-- or when another "sender app" takes over the ChromeCast.
cast' :: ERL r => Text -> Eff r ()
cast' f = do
  chronicle Info $ "Casting " <> f
  r <- lift . maybeSh . asyncSh $ run_ "castnow" [f, "--quiet"]
  case r of
    Nothing -> throwExc ("Failed to stream to ChromeCast." :: Text)
    Just _  -> pure ()  -- Dumps the Async and returns right away.

-- | Execute a Shelly command, smothering any thrown exceptions in `Maybe`.
maybeSh :: Sh a -> IO (Maybe a)
maybeSh s = catchany (Just <$> shelly s) (\_ -> pure Nothing)
