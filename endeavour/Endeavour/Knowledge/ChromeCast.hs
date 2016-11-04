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
  ) where

import           Control.Concurrent
import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Control.Monad (void)
import           Data.Monoid
import           Data.Text (Text)
import           Endeavour.Genetics
import           Endeavour.Memory
import           Lens.Micro
import           Shelly
import qualified Text.Fuzzy as F

---

-- | ChromeCast can handle MP4, MKV, and normal audio formats (including FLAC)
-- natively. AVIs will be converted on-the-fly via the @--tomp4@ flag.
--
-- @castnow@ will exit successfully when the file is done playing,
-- or when another "sender app" takes over the ChromeCast.
cast :: ERL r => Text -> Eff r ()
cast f = do
  toCast <- fileToCast f
  chronicle Info $ "Casting " <> toCast
  void . effShelly "Failed to stream to ChromeCast." . asyncSh $ do
    run_ "castnow" [toCast, "--quiet"]
    liftIO $ putStrLn "Yeah!!!"
    liftIO (myThreadId >>= killThread) -- Kill thread after casting completes.

-- | Execute a Shelly command, smothering any thrown exceptions in `Maybe`.
maybeSh :: Sh a -> IO (Maybe a)
maybeSh s = catchany (Just <$> shelly s) (\_ -> pure Nothing)

-- | Catch any Shelly exception and rethrow with a given message.
effShelly :: ERL r => Text -> Sh a -> Eff r a
effShelly e s = lift (maybeSh s) >>= maybe (throwExc e) pure

-- | Given some suggestion text, finds a suitable media file to cast,
-- if it can.
fileToCast :: ERL r => Text -> Eff r Text
fileToCast t = ((++) <$> audio <*> video) >>= liftEither . fileToCast' t

-- | Given its own fuction to isolate pure code, as well as hand-hold EE.
fileToCast' :: Text -> [Text] -> Either Text Text
fileToCast' t fs = maybe (Left "No suitable media file found.") Right (F.simpleFilter t fs ^? _head)

-- | All audio files. Assumes that each file in `audio/` is a directory,
-- and that each directory has no further subdirectories.
audio :: RL r => Eff r [Text]
audio = reader _media >>= lift . shelly . f
  where f p = concat <$> (ls (p </> ("audio" :: Text)) >>= mapM lsT)

-- | All video files.
video :: RL r => Eff r [Text]
video = reader _media >>= lift . shelly . lsT . (</> ("video" :: Text))
