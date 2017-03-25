{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Endeavour.Console.Types where

import           Brick
import           Brick.Widgets.List
import           Data.Text (Text)
import qualified Deque as D
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast (Media)
import           Endeavour.Knowledge.Hue hiding (lights)
import           Endeavour.Memory
import           Lens.Micro.TH

---

-- | All resource names.
data RName = LGroupList | MediaList | LogList | AlbumTracks deriving (Eq, Show, Ord)

-- | Possible application pages.
data Page = Lights | Media | Logs deriving (Eq, Enum, Show)

-- | The application state.
data System = System { _env         :: Env
                     , _msg         :: Text
                     , _pages       :: D.Deque Page
                     , _lightGroups :: List RName (ID, Group)
                     , _mediaFiles  :: List RName Media
                     , _albumTracks :: List RName Text
                     , _trackView   :: Bool
                     , _logEntries  :: List RName Log }
makeLenses ''System

selected :: AttrName
selected = attrName "selected"

logWarn :: AttrName
logWarn = attrName "warn"

logFail :: AttrName
logFail = attrName "fail"

onAttr :: AttrName
onAttr = attrName "lightOn"

offAttr :: AttrName
offAttr = attrName "lightOff"

albumAttr :: AttrName
albumAttr = attrName "album"
