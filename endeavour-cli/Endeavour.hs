{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Control.Monad (void)
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Deque as D
import qualified Graphics.Vty as G
import           Lens.Micro
import           Lens.Micro.TH
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | All resource names.
data RName = MediaList deriving (Eq, Show, Ord)

-- | Possible application pages.
data Page = Lights | Media | Logs deriving (Eq, Enum, Show)

-- | The application state.
data System = System { _msg         :: T.Text
                     , _pages       :: D.Deque Page
                     , _mediaFiles  :: List RName T.Text } deriving (Show)
makeLenses ''System

selected :: AttrName
selected = attrName "selected"

boxy :: Widget n -> Widget n
boxy = withBorderStyle unicodeRounded . border . padAll 5

widgets :: System -> Widget RName
widgets s = center (boxy . page s . fromJust . D.head $ _pages s) <=> footer s
 --centerWith (Just '.') boxy <=> footer s

lights :: Widget n
lights = txt "Light Controls"

media :: List RName T.Text -> Widget RName
media = vLimit 30 . hLimit 30 . renderList f False
  where f False e = txt e
        f True e = withAttr selected $ txt e

logs :: Widget n
logs = txt "Log Display"

-- | Dispatch a `Widget` based on the selected `Page`.
-- This is to turn pages/tabs in the app.
page :: System -> Page -> Widget RName
page _ Lights = lights
page s Media = media $ _mediaFiles s
page _ Logs = logs

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

-- | The application event handler. Resizing still happens automatically.
handle :: System -> BrickEvent t t1 -> EventM RName (Next System)
handle s (VtyEvent (G.EvKey G.KEsc _)) = halt s
handle s (VtyEvent (G.EvKey G.KLeft [G.MShift])) = continue (s & msg .~ "<==" & pages %~ D.shiftRight)
handle s (VtyEvent (G.EvKey G.KRight [G.MShift])) = continue (s & msg .~ "==>" & pages %~ D.shiftLeft)
handle s e = case fromJust . D.head $ _pages s of
  Lights -> lightHandle s e
  Media  -> mediaHandle s e
  Logs   -> logHandle s e

lightHandle :: s -> t -> EventM n (Next s)
lightHandle s e = continue s

-- We'll have to make a call to the backend API, and unwrap the result
-- each time, saving the key parts (db connection?) back into the `System` state.
-- `liftIO` will be our friend here.
-- | Handle events unique to the Media page.
mediaHandle :: System -> BrickEvent t t1 -> EventM RName (Next System)
mediaHandle s (VtyEvent (G.EvKey G.KEnter _)) = case listSelectedElement $ _mediaFiles s of
  Nothing -> continue s
  Just (_, f) -> continue (s & msg .~ f)  -- TODO Dispatch media request.
mediaHandle s (VtyEvent e) = handleEventLensed s mediaFiles handleListEvent e >>= continue

logHandle :: s -> t -> EventM n (Next s)
logHandle s e = continue s

-- | A description of how to run our application.
app :: App System () RName
app = App { appDraw = \s -> [ui s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handle
          , appStartEvent = pure
          , appAttrMap = const $ attrMap G.defAttr [ (progressCompleteAttr, bg G.blue)
                                                   , (selected, bg G.blue)
                                                   ]
          }

-- TODO Get media list here via Shelly. Media dir should be in Endeavour `Env`.
main :: IO ()
main = do
  user <- T.pack <$> getEffectiveUserName
  let m = [st|Hello, %s.|] (T.toTitle user)
      p = D.fromList [Lights ..]
      l = list MediaList (V.fromList $ T.words "This is a sample list") 5
  void . defaultMain app $ System m p l
  putStrLn "Shutdown complete."
