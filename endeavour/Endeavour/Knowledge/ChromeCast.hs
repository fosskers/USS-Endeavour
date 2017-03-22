{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Endeavour.Knowledge.ChromeCast
-- Copyright : (c) Colin Woodbury, 2016 - 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Functions for interacting with a ChromeCast.

module Endeavour.Knowledge.ChromeCast
  ( -- * Types
    Media(..)
  -- * Commands
  , cast, pause, unpause, stop
    -- * Resources
  , albums, videos
  ) where

import           Control.Monad (void)
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import           Endeavour.Genetics
import           Endeavour.Memory
import           Lens.Micro
import           Shelly
import qualified Text.Fuzzy as F

---

-- | An audio album with its name and tracklist, or a video file with its path.
data Media = Album T.Text [T.Text] | Video T.Text

-- | ChromeCast can handle MP4, MKV, and normal audio formats (including FLAC)
-- natively. AVIs will be converted on-the-fly via the @-transcode@ flag.
--
-- @stream2chromecast@ will exit successfully when the file is done playing,
-- or when another "sender app" takes over the ChromeCast.
cast :: ERL r => T.Text -> Eff r ()
cast f = do
  toCast <- fileToCast f
  chronicle Info $ "Casting " <> toCast
  sheff "Failed to stream to ChromeCast." $ run_ "stream2chromecast" (castArgs toCast)

-- | If the media file is an AVI, we need to transcode it.
castArgs :: T.Text -> [T.Text]
castArgs f = case T.breakOnEnd "." f of
  (_, "avi") -> ["-transcode", f]
  _ -> [f]

pause :: ERL r => Eff r ()
pause = do
  chronicle Info "Pausing Chromecast."
  sheff "Couldn't pause the Chromecast." $ run_ "stream2chromecast" ["-pause"]

unpause :: ERL r => Eff r ()
unpause = do
  chronicle Info "Resuming paused Chromecast."
  sheff "Couldn't resume the paused Chromecast." $ run_ "stream2chromecast" ["-continue"]

stop :: ERL r => Eff r ()
stop = do
  chronicle Info "Stopping the Chromecast."
  sheff "Couldn't stop the Chromecast." $ run_ "stream2chromecast" ["-stop"]

-- | Silently and asynchronously run a Shell command.
sheff :: ERL r => T.Text -> Sh a -> Eff r ()
sheff t s = void . effShelly t . asyncSh . print_stderr False $ print_stdout False s

-- | Execute a Shelly command, smothering any thrown exceptions in `Maybe`.
maybeSh :: Sh a -> IO (Maybe a)
maybeSh s = catchany (Just <$> shelly s) (\_ -> pure Nothing)

-- | Catch any Shelly exception and rethrow with a given message.
effShelly :: ERL r => T.Text -> Sh a -> Eff r a
effShelly e s = send (maybeSh s) >>= maybe (throwError e) pure

-- | Given some suggestion text, finds a suitable media file to cast,
-- if it can.
fileToCast :: ERL r => T.Text -> Eff r T.Text
fileToCast t = ((++) <$> audio <*> video) >>= liftEither . fileToCast' t

-- | Given its own fuction to isolate pure code, as well as hand-hold EE.
fileToCast' :: T.Text -> [T.Text] -> Either T.Text T.Text
fileToCast' t fs = maybe (Left "No suitable media file found.") Right (F.simpleFilter t fs ^? _head)

-- | All audio files.
audio :: RL r => Eff r [T.Text]
audio = asks _media >>= send . shelly @IO . f
  where f p = concat <$> (ls (p </> ("audio" :: T.Text)) >>= mapM lsT)

-- | Every album with its tracklist. Assumes that each file in @audio/@ is a
-- directory, and that each directory has no further subdirectories.
albums :: RL r => Eff r [Media]
albums = f <$> asks _media <*> audio
  where f fp fs = map h . groupBy (\(a,_) (b,_) -> a == b) . sort $ map (g fp) fs
        g fp t = maybe ("",t) (T.breakOn "/") $ T.stripPrefix (fp <> "audio/") t
        h ps = Album (fst $ head ps) $ map snd ps

video :: RL r => Eff r [T.Text]
video = asks _media >>= send . shelly @IO . lsT . (</> ("video" :: T.Text))

-- | All video files.
videos :: RL r => Eff r [Media]
videos = map Video . sort <$> video
