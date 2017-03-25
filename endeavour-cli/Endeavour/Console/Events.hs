{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Endeavour.Console.Events where

import           Brick
import           Brick.Widgets.List
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Deque as D
import           Endeavour.Console.Types
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast
import           Endeavour.Knowledge.Hue hiding (lights)
import qualified Graphics.Vty as G
import           Lens.Micro
import           Text.Printf.TH

---

-- | The application event handler. Resizing still happens automatically.
handle :: System -> BrickEvent t t1 -> EventM RName (Next System)
handle s (VtyEvent (G.EvKey G.KEsc _)) = halt s
handle s (VtyEvent (G.EvKey G.KLeft [G.MShift])) = continue (s & msg .~ "<==" & pages %~ D.shiftRight)
handle s (VtyEvent (G.EvKey G.KRight [G.MShift])) = continue (s & msg .~ "==>" & pages %~ D.shiftLeft)
handle s e = case fromJust . D.head $ _pages s of
  Lights -> lightH s e
  Media  -> mediaH s e
  Logs   -> logH s e

-- TODO Refactor using `eff`.
-- | Handle events unique to the Lights page.
lightH :: System -> BrickEvent t t1 -> EventM RName (Next System)
lightH s (VtyEvent (G.EvKey G.KEnter _)) = case listSelectedElement $ _lightGroups s of
  Nothing -> continue s
  Just (_, (i, g)) -> do
    liftIO . runEffect (_env s) $ overGroup lightOn i
    continue (s & msg .~ [st|ON: %s|] (_gname g)
                & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOn))
lightH s (VtyEvent (G.EvKey G.KBS _)) = case listSelectedElement $ _lightGroups s of
  Nothing -> continue s
  Just (_, (i, g)) -> do
    liftIO . runEffect (_env s) $ overGroup lightOff i
    continue (s & msg .~ [st|OFF: %s|] (_gname g)
                & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOff))
lightH s (VtyEvent e) = handleEventLensed s lightGroups handleListEvent e >>= continue

-- | Handle events unique to the Media page.
mediaH :: System -> BrickEvent t t1 -> EventM RName (Next System)
--medi s (VtyEvent (G.EvKey G.KEnter _)) = listEff s (_mediaFiles s) cast id
mediaH s (VtyEvent (G.EvKey (G.KChar 'p') _)) = eff s pause "Pausing ChromeCast."
mediaH s (VtyEvent (G.EvKey (G.KChar 'c') _)) = eff s unpause "Unpausing ChromeCast."
mediaH s (VtyEvent (G.EvKey (G.KChar 's') _)) = eff s stop "Stopping ChromeCast."
mediaH s (VtyEvent (G.EvKey (G.KChar '\t') _)) = continue $ tabH s
mediaH s (VtyEvent e) | _trackView s = handleEventLensed s albumTracks handleListEvent e >>= continue
                      | otherwise = handleEventLensed s mediaFiles handleListEvent e >>= continue

-- | Decide where to move the cursor focus.
tabH :: System -> System
tabH s | _trackView s = s & trackView .~ False
       | otherwise = pushAlbumTracks s

-- | Display a TAB'd Album's tracks in the tracks list.
pushAlbumTracks :: System -> System
pushAlbumTracks s = case listSelectedElement $ _mediaFiles s of
  Nothing -> s
  Just (_, Video _) -> s & msg .~ "Can't expand - that's a video file."
  Just (_, Album t ts) -> s & albumTracks %~ listReplace (V.fromList ts) (Just 0)
                            & trackView .~ True
                            & msg .~ "Displaying tracks for: " <> t

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
logH :: System -> BrickEvent t t1 -> EventM RName (Next System)
logH s (VtyEvent e) = handleEventLensed s logEntries handleListEvent e >>= continue
