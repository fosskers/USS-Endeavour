{-# LANGUAGE OverloadedStrings #-}

module Endeavour.Memory
  ( Log(..)
  , LogCat(..)
  , chronicle
  ) where

import qualified Data.Text as T
import           Data.Time.Clock
import           Database.SQLite.Simple

---

{- NOTES:

`concurrently` and `withConnection` do not play well together. sqlite will
throw a "database is locked" IO exception.

With a single application-wide `Connection`, concurrent access to sqlite is
possible. Concurrent writes don't clobber each other, the underlying engine
seems to take care of that.

-}

data LogCat = Info | Warn | Fail deriving (Read, Show)

-- | A log of some event aboard the Ship.
data Log = Log UTCTime LogCat T.Text deriving (Show)

instance FromRow Log where
  fromRow = Log <$> field <*> (fmap read field) <*> field

instance ToRow Log where
  toRow (Log time cat text) = toRow (time, show cat, text)

-- | Log some event message.
chronicle :: Connection -> LogCat -> T.Text -> IO ()
chronicle c cat t = do
  now <- getCurrentTime
  execute c "INSERT INTO shiplog (dt, cat, log) VALUES (?, ?, ?)" $ Log now cat t

-- | Probe the Ship's memory for event logs. An optional limit factor can be
-- supplied.
recall :: Connection -> Maybe Int -> IO [Log]
recall c Nothing  = query_ c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC;"
recall c (Just n) = query c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC LIMIT ?;" $ Only n

{-}
say :: IO [()]
say = withConnection "/home/colin/code/endeavour/shiplog.db" $ \c -> do
  execute_ c "CREATE TABLE IF NOT EXISTS shiplog (id INTEGER PRIMARY KEY, dt DATETIME, cat TEXT, log TEXT)"
  mapConcurrently (chronicle c Info) (T.words "I should make this a very long message")

hear :: IO [Log]
hear = withConnection "/home/colin/code/endeavour/shiplog.db" $ \c -> do
  recall c $ Just 2
-}
