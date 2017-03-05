{-# LANGUAGE OverloadedStrings #-}

module Endeavour.Console.UI where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import qualified Deque as D
import           Endeavour.Console.Types
import           Endeavour.Knowledge.Hue hiding (lights)
import           Endeavour.Memory

---

boxy :: Widget n -> Widget n
boxy = padAll 5

widgets :: System -> Widget RName
widgets s = center (boxy . page s . fromJust . D.head $ _pages s) <=> footer s

lights :: List RName (t, Group) -> Widget RName
lights = renderList f False
  where f b (_,g) = bracket (o g) <+> txt " " <+> h b g
        h False g = txt $ _gname g
        h True g = withAttr selected . txt $ _gname g
        o g | isOn $ _gaction g = withAttr onAttr $ txt "ON "
            | otherwise = withAttr offAttr $ txt "OFF"

media :: List RName T.Text -> Widget RName
media = renderList f False
  where f False e = txt e
        f True e = withAttr selected $ txt e

logs :: List RName Log -> Widget RName
logs = renderList f False
  where f b (Log t c e) = hBox $ map (padRight $ Pad 1) [bracket (logCat c), time t, txt "==>", g b e]
        g True w = withAttr selected (txt w)
        g False w = txt w

time :: UTCTime -> Widget n
time = str . formatTime defaultTimeLocale "%Y-%m-%d (%a) %H:%M:%S"

-- | Colour a `LogCat`.
logCat :: LogCat -> Widget n
logCat Warn = withAttr logWarn (txt "Warn")
logCat Fail = withAttr logFail (txt "Fail")
logCat lc = str $ show lc

bracket :: Widget n -> Widget n
bracket w = txt "[" <+> w <+> txt "]"

-- | Dispatch a `Widget` based on the selected `Page`.
-- This is to turn pages/tabs in the app.
page :: System -> Page -> Widget RName
page s Lights = lights $ _lightGroups s
page s Media = media $ _mediaFiles s
page s Logs = logs $ _logEntries s

footer :: System -> Widget n
footer s = hBox
  [ padRight Max . padLeft (Pad 1) . txt $ _msg s --progressBar (Just "Progress") 0.4
  , padLeft Max . padRight (Pad 1) $ rights ]
  where curr = fromJust . D.head $ _pages s
        rights = foldl1 (<+>) . intersperse (txt " | ") $ map f [Lights ..]
        f p | p == curr = withAttr selected . txt . T.pack $ show p
            | otherwise = txt . T.pack $ show p

-- | The final conglomeration of `Widget`s.
ui :: System -> Widget RName
ui s = withBorderStyle unicodeBold . borderWithLabel (padLeftRight 1 $ txt "Endeavour System Controls") $ widgets s
