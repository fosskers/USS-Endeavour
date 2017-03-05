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
import           Control.Eff.Lift
import           Control.Eff.Reader.Lazy
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intersperse, sort)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import qualified Data.Vector as V
import qualified Deque as D
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast
import           Endeavour.Knowledge.Hue hiding (lights)
import           Endeavour.Knowledge.Space
import           Endeavour.Memory
import qualified Graphics.Vty as G
import           Lens.Micro
import           Lens.Micro.TH
import           Options.Generic
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | Command-line arguments.
newtype Args = Args { config :: FilePath } deriving (Generic, ParseRecord)

-- | All resource names.
data RName = LGroupList | MediaList | LogList deriving (Eq, Show, Ord)

-- | Possible application pages.
data Page = Lights | Media | Logs deriving (Eq, Enum, Show)

-- | The application state.
data System = System { _env         :: Env
                     , _msg         :: T.Text
                     , _pages       :: D.Deque Page
                     , _lightGroups :: List RName (ID, Group)
                     , _mediaFiles  :: List RName T.Text
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

-- | The application event handler. Resizing still happens automatically.
handle :: System -> BrickEvent t t1 -> EventM RName (Next System)
handle s (VtyEvent (G.EvKey G.KEsc _)) = halt s
handle s (VtyEvent (G.EvKey G.KLeft [G.MShift])) = continue (s & msg .~ "<==" & pages %~ D.shiftRight)
handle s (VtyEvent (G.EvKey G.KRight [G.MShift])) = continue (s & msg .~ "==>" & pages %~ D.shiftLeft)
handle s e = case fromJust . D.head $ _pages s of
  Lights -> lightHandle s e
  Media  -> mediaHandle s e
  Logs   -> logHandle s e

-- TODO Refactor using `eff`.
-- | Handle events unique to the Lights page.
lightHandle :: System -> BrickEvent t t1 -> EventM RName (Next System)
lightHandle s (VtyEvent (G.EvKey G.KEnter _)) = case listSelectedElement $ _lightGroups s of
  Nothing -> continue s
  Just (_, (i, g)) -> do
    liftIO . runEffect (_env s) $ overGroup lightOn i
    continue (s & msg .~ [st|ON: %s|] (_gname g)
                & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOn))
lightHandle s (VtyEvent (G.EvKey G.KBS _)) = case listSelectedElement $ _lightGroups s of
  Nothing -> continue s
  Just (_, (i, g)) -> do
    liftIO . runEffect (_env s) $ overGroup lightOff i
    continue (s & msg .~ [st|OFF: %s|] (_gname g)
                & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOff))
lightHandle s (VtyEvent e) = handleEventLensed s lightGroups handleListEvent e >>= continue

-- | Handle events unique to the Media page.
mediaHandle :: System -> BrickEvent t t1 -> EventM RName (Next System)
mediaHandle s (VtyEvent (G.EvKey G.KEnter _)) = listEff s (_mediaFiles s) cast id
mediaHandle s (VtyEvent (G.EvKey (G.KChar 'p') _)) = eff s pause "Pausing ChromeCast."
mediaHandle s (VtyEvent (G.EvKey (G.KChar 'c') _)) = eff s unpause "Unpausing ChromeCast."
mediaHandle s (VtyEvent (G.EvKey (G.KChar 's') _)) = eff s stop "Stopping ChromeCast."
mediaHandle s (VtyEvent e) = handleEventLensed s mediaFiles handleListEvent e >>= continue

-- | Perform some action based on a `List`'s selected element.
listEff :: System -> List n1 t -> (t -> Effect b) -> (t -> Text) -> EventM n (Next System)
listEff s l e t = maybe (continue s) (\(_,i) -> eff s (e i) (t i)) $ listSelectedElement l

-- | Run an `Effect` within the `EventM` context, displaying debug messages
-- as necessary.
eff :: System -> Effect b -> Text -> EventM n (Next System)
eff s e t = do
  res <- liftIO $ runEffect (_env s) e
  continue $ either (\err -> s & msg .~ err) (\_ -> s & msg .~ t) res

-- | Handle events unique to the Log page.
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
                                                   , (onAttr, bg G.yellow)
                                                   , (offAttr, bg G.red)
                                                   ]
          }

main :: IO ()
main = do
  Args c <- getRecord "U.S.S. Endeavour - Operation Terminal"
  env <- awaken c
  case env of
    Nothing -> putStrLn "Failed to parse config file."
    Just e  -> do
      chronicle' (_conn e) Info "Starting CLI client."
      grps <- either (const []) M.toList <$> runEffect e groups
      vids <- runLift $ runReader video e
      audi <- runLift $ runReader audio e
      logs <- runLift $ runReader (recall Nothing) e
      user <- T.pack <$> getEffectiveUserName
      astr <- either (const 0) length <$> runEffect e astronauts
      let m = [st|Hello, %s. There are currently %d humans in space.|] (T.toTitle user) astr
          p = D.fromList [Lights ..]
          h = list LGroupList (V.fromList grps) 1
          v = list MediaList (V.fromList $ sort (vids ++ audi)) 1
          l = list LogList (V.fromList logs) 1
      void . defaultMain app $ System e m p h v l
      slumber e
      putStrLn "Shutdown complete."
