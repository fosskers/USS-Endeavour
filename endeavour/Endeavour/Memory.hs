{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Endeavour.Memory
-- Copyright : (c) Colin Woodbury, 2016 - 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Code for interacting with the Endeavour's memory banks,
-- backed by <https://www.sqlite.org/ SQLite>.

module Endeavour.Memory
  ( Log(..)
  , LogCat(..)
  , wake
  , chronicle
  , chronicle'
  , recall
  ) where

import           Data.Aeson
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Word
import           Database.SQLite.Simple
import           Endeavour.Genetics

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
  fromRow = Log <$> field <*> fmap read field <*> field

instance ToRow Log where
  toRow (Log time cat text) = toRow (time, show cat, text)

instance ToJSON Log where
  toJSON (Log time cat text) = object [ "time" .= time, "category" .= show cat, "text" .= text ]

-- | Create the SQLite table for logs, if necessary.
wake :: Connection -> IO ()
wake c = execute_ c "CREATE TABLE IF NOT EXISTS shiplog (id INTEGER PRIMARY KEY, dt DATETIME, cat TEXT, log TEXT)"

-- | Log some event message.
chronicle :: RL r => LogCat -> T.Text -> Eff r ()
chronicle cat t = do
  c <- asks _conn
  send $ chronicle' c cat t

-- | Log some event message via the `IO` Monad.
chronicle' :: Connection -> LogCat -> T.Text -> IO ()
chronicle' conn cat t = do
  now <- getCurrentTime
  execute conn "INSERT INTO shiplog (dt, cat, log) VALUES (?, ?, ?)" $ Log now cat t

-- | Probe the Ship's memory for event logs. An optional limit factor can be
-- supplied.
recall :: RL r => Maybe Word16 -> Eff r [Log]
recall m = asks _conn >>= send . f m
  where f Nothing  c = query_ c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC;"
        f (Just n) c = query c "SELECT dt, cat, log FROM shiplog ORDER BY dt DESC LIMIT ?;" $ Only n
