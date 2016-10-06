{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Endeavour.Memory
  ( Log(..)
  , LogCat(..)
  , wake
  , chronicle
  , recall
  ) where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.SQLite.Simple
import           Endeavour.Types

---

{- NOTES:

`concurrently` and `withConnection` do not play well together. sqlite will
throw a "database is locked" IO exception.

With a single application-wide `Connection`, concurrent access to sqlite is
possible. Concurrent writes don't clobber each other, the underlying engine
seems to take care of that.

-}

-- | A log category.
data LogCat = Info | Warn | Fail deriving (Eq, Read, Show)

-- | A log of some event aboard the Ship.
data Log = Log UTCTime LogCat T.Text deriving (Show)

instance FromRow Log where
  fromRow = Log <$> field <*> (fmap read field) <*> field

instance ToRow Log where
  toRow (Log time cat text) = toRow (time, show cat, text)

-- | Create the SQLite table for logs, if necessary.
wake :: RIO r => Eff r ()
wake = do
  c <- reader _conn
  lift $ execute_ c "CREATE TABLE IF NOT EXISTS shiplog (id INTEGER PRIMARY KEY, dt DATETIME, cat TEXT, log TEXT)"

-- | Log some event message.
chronicle :: RIO r => LogCat -> T.Text -> Eff r ()
chronicle cat t = do
  c <- reader _conn
  now <- lift getCurrentTime
  lift $ execute c "INSERT INTO shiplog (dt, cat, log) VALUES (?, ?, ?)" $ Log now cat t

-- | Probe the Ship's memory for event logs. An optional limit factor can be
-- supplied.
recall :: RIO r => Maybe Int -> Eff r [Log]
recall m = reader _conn >>= lift . f m
  where f Nothing  c = query_ c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC;"
        f (Just n) c = query c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC LIMIT ?;" $ Only n
