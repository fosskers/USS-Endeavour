{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import qualified Data.Vector as V
import qualified Deque as D
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast
import           Endeavour.Memory
import qualified Graphics.Vty as G
import           Lens.Micro
import           Lens.Micro.TH
import           Options.Generic
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | Command-line arguments.
data Args = Args { config :: FilePath } deriving (Generic, ParseRecord)

-- | All resource names.
data RName = MediaList | LogList deriving (Eq, Show, Ord)

-- | Possible application pages.
data Page = Lights | Media | Logs deriving (Eq, Enum, Show)

-- | The application state.
data System = System { _env         :: Env
                     , _msg         :: T.Text
                     , _pages       :: D.Deque Page
                     , _mediaFiles  :: List RName T.Text
                     , _logEntries  :: List RName Log }
makeLenses ''System

selected :: AttrName
selected = attrName "selected"

logWarn :: AttrName
logWarn = attrName "warn"

logFail :: AttrName
logFail = attrName "fail"

boxy :: Widget n -> Widget n
boxy = padAll 5
--withBorderStyle unicodeRounded . border . padAll 5

widgets :: System -> Widget RName
widgets s = center (boxy . page s . fromJust . D.head $ _pages s) <=> footer s
 --centerWith (Just '.') boxy <=> footer s

lights :: Widget n
lights = txt "Light Controls"

media :: List RName T.Text -> Widget RName
media = renderList f False --vLimit 30 . hLimit 80 . renderList f False
  where f False e = txt e
        f True e = withAttr selected $ txt e

logs :: List RName Log -> Widget RName
logs = renderList f False --txt "Log Display"
  where f False (Log t c e) = hBox $ map (padRight $ Pad 1) [bracket (logCat c), time t, txt "==>", txt e]
        f True (Log t c e) = hBox $ map (padRight $ Pad 1) [bracket (logCat c), time t, txt "==>", withAttr selected (txt e)]

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
page _ Lights = lights
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

-- TODO Put this in Endeavour backend.
doIt :: Env -> Effect a -> IO (Either Text a)
doIt env eff = runLift . runExc $ runReader eff env

-- | Handle events unique to the Media page.
mediaHandle :: System -> BrickEvent t t1 -> EventM RName (Next System)
mediaHandle s (VtyEvent (G.EvKey G.KEnter _)) = case listSelectedElement $ _mediaFiles s of
  Nothing -> continue s
  Just (_, f) -> do
    liftIO $ doIt (_env s) (cast f)  -- TODO Pattern match on `Either` for errors
    continue (s & msg .~ f)
mediaHandle s (VtyEvent e) = handleEventLensed s mediaFiles handleListEvent e >>= continue

logHandle :: System -> BrickEvent t t1 -> EventM RName (Next System)
logHandle s (VtyEvent e) = handleEventLensed s logEntries handleListEvent e >>= continue

-- | A description of how to run our application.
app :: App System () RName
app = App { appDraw = \s -> [ui s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handle
          , appStartEvent = pure
          , appAttrMap = const $ attrMap G.defAttr [ (progressCompleteAttr, bg G.blue)
                                                   , (selected, bg G.blue)
                                                   , (logWarn, bg G.yellow)
                                                   , (logFail, bg G.red)
                                                   ]
          }

-- TODO Get media list here via Shelly. Media dir should be in Endeavour `Env`.
main :: IO ()
main = do
  Args c <- getRecord "U.S.S. Endeavour - Operation Terminal"
  env <- awaken c
  case env of
    Nothing -> putStrLn "Failed to parse config file."
    Just e  -> do
      vids <- runLift $ runReader video e
      logs <- runLift $ runReader (recall Nothing) e
      user <- T.pack <$> getEffectiveUserName
      let m = [st|Hello, %s.|] (T.toTitle user)
          p = D.fromList [Lights ..]
          v = list MediaList (V.fromList vids) 1 -- $ T.words "This is a sample list") 5
          l = list LogList (V.fromList logs) 1
      void . defaultMain app $ System e m p v l
      slumber e
      putStrLn "Shutdown complete."
