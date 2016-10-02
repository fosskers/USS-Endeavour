{-# LANGUAGE OverloadedStrings #-}

module Endeavour.Memory where

import qualified Data.Text as T
import           Data.Time.Clock
import           Database.SQLite.Simple

---

-- | A log of some event aboard the Ship.
data Log = Log UTCTime T.Text deriving (Show)

instance FromRow Log where
  fromRow = Log <$> field <*> field

instance ToRow Log where
  toRow (Log time text) = toRow (time, text)

-- | Log some event message.
log :: Connection -> T.Text -> IO ()
log c t = do
  now <- getCurrentTime
  execute c "INSERT INTO shiplog (dt, log) VALUES (?, ?)" $ Log now t
