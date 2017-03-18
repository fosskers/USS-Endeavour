{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Endeavour.Console.Events where

import           Brick
import           Brick.Widgets.List
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
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