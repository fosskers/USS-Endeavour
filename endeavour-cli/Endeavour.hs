{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.ProgressBar
import           Control.Monad (void)
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Deque as D
import qualified Graphics.Vty as V
import           Lens.Micro
import           Lens.Micro.TH
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | Possible application pages.
data Page = Lights | Media | Logs deriving (Eq, Enum, Show)

-- | The application state.
data System = System { _msg :: T.Text, _pages :: D.Deque Page } deriving (Eq, Show)
makeLenses ''System

-- | All resource names.
data Name = Jack deriving (Eq, Show, Ord)

selected :: AttrName
selected = attrName "selected"

boxy :: Widget n
boxy = withBorderStyle unicodeRounded . border . padAll 5 $ txt "Welcome, Officer."

widgets :: System -> Widget n
widgets s = centerWith (Just '.') boxy <=> footer s

footer :: System -> Widget n
footer s = hBox
  [ padRight Max . padLeft (Pad 1) . txt $ _msg s --progressBar (Just "Progress") 0.4
  , padLeft Max . padRight (Pad 1) $ rights ]
  where curr = fromJust . D.head $ _pages s
        rights = foldl1 (<+>) . intersperse (txt " | ") $ map f [Lights ..]
        f p | p == curr = withAttr selected . txt . T.pack $ show p
            | otherwise = txt . T.pack $ show p

-- | The final conglomeration of `Widget`s.
ui :: System -> Widget n
ui s = withBorderStyle unicodeBold . borderWithLabel (padLeftRight 1 $ txt "Endeavour System Controls") $ widgets s

-- | The application event handler. Resizing still happens automatically.
handle :: System -> BrickEvent t t1 -> EventM n (Next System)
handle s (VtyEvent (V.EvKey V.KEsc _)) = halt s
handle s (VtyEvent (V.EvKey (V.KChar c) _)) = continue (s & msg .~ [st|Key: %c|] c)
handle s (VtyEvent (V.EvKey V.KLeft [V.MShift])) = continue (s & msg .~ "<==" & pages %~ D.shiftRight)
handle s (VtyEvent (V.EvKey V.KLeft _)) = continue (s & msg .~ "<--")
handle s (VtyEvent (V.EvKey V.KRight [V.MShift])) = continue (s & msg .~ "==>" & pages %~ D.shiftLeft)
handle s (VtyEvent (V.EvKey V.KRight _)) = continue (s & msg .~ "-->")
handle s (VtyEvent (V.EvKey V.KDown _)) = continue (s & msg .~ "vvv")
handle s (VtyEvent (V.EvKey V.KUp _)) = continue (s & msg .~ "^^^")
handle s (VtyEvent (V.EvKey V.KEnter _)) = continue (s & msg .~ "ENTER")
handle s _ = continue (s & msg .~ "Warning: Uncaught VtyEvent")

-- | A description of how to run our application.
app :: App System () Name
app = App { appDraw = \s -> [ui s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handle
          , appStartEvent = pure
          , appAttrMap = const $ attrMap V.defAttr [ (progressCompleteAttr, bg V.blue)
                                                   , (selected, bg V.blue)
                                                   ]
          }

main :: IO ()
main = do
  user <- T.pack <$> getEffectiveUserName
  let m = [st|Hello, %s.|] (T.toTitle user)
      p = D.fromList [Lights ..]
  void . defaultMain app $ System m p
  putStrLn "Shutdown complete."
