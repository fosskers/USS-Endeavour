{-# LANGUAGE OverloadedStrings #-}

module Endeavour.Console.UI ( ui ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import qualified Deque as D
import           Endeavour.Console.Types
import           Endeavour.Genetics
import           Endeavour.Knowledge.Hue hiding (lights)
import           Endeavour.Memory
import           Lens.Micro

---

-- | The final conglomeration of `Widget`s.
ui :: System -> Widget RName
ui s = boldBorder "Endeavour System Controls" $ widgets s

widgets :: System -> Widget RName
widgets s = center (boxy . page s . fromJust . D.head $ _pages s) <=> footer s

-- | Put a border with title text around some `Widget`.
boldBorder :: T.Text -> Widget n -> Widget n
boldBorder t w = withBorderStyle unicodeBold $ borderWithLabel (padLeftRight 1 $ txt t) w

boxy :: Widget n -> Widget n
boxy = padAll 5

lights :: List RName (t, Group) -> Widget RName
lights = renderList f False
  where f b (_,g) = horiz [ bracket (o g), h b g ]
        h False g = txt $ _gname g
        h True g = withAttr selected . txt $ _gname g
        o g | isOn $ _gaction g = withAttr onAttr $ txt "ON "
            | otherwise = withAttr offAttr $ txt "OFF"

media :: T.Text -> List RName T.Text -> Widget RName
media path = renderList f False
  where f False e = box $ shorten e
        f True e = withAttr selected . box $ shorten e
        shorten t = maybe t id $ T.stripPrefix path t
        box t = case T.splitAt 5 t & _2 %~ T.drop 1 of
          ("audio", file) -> horiz [ bracket (txt "audio"), album file ]
          ("video", file) -> horiz [ bracket (txt "video"), txt file ]
        album t = case T.breakOn "/" t & _2 %~ T.drop 1 of
          (alb, file) -> txt $ "[" <> alb <> "] " <> file

logs :: List RName Log -> Widget RName
logs = renderList f False
  where f b (Log t c e) = horiz [bracket (logCat c), time t, txt "==>", g b e]
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

-- | Align a collection of Widgets together horizontally, with a space
-- between each one.
horiz :: [Widget n] -> Widget n
horiz = hBox . map (padRight $ Pad 1)

-- | Dispatch a `Widget` based on the selected `Page`.
-- This is to turn pages/tabs in the app.
page :: System -> Page -> Widget RName
page s Lights = lights $ _lightGroups s
page s Media = media (_media $ _env s) $ _mediaFiles s
page s Logs = logs $ _logEntries s

footer :: System -> Widget n
footer s = hBox
  [ padRight Max . padLeft (Pad 1) . txt $ _msg s --progressBar (Just "Progress") 0.4
  , padLeft Max . padRight (Pad 1) $ rights ]
  where curr = fromJust . D.head $ _pages s
        rights = foldl1 (<+>) . intersperse (txt " | ") $ map f [Lights ..]
        f p | p == curr = withAttr selected . txt . T.pack $ show p
            | otherwise = txt . T.pack $ show p
