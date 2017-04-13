{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Endeavour.Console.Events ( handle ) where

import           Brick
import           Brick.BChan
import           Brick.Widgets.List
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (toList)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Deque as D
import           Endeavour.Console.Types
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast
import           Endeavour.Knowledge.Hue hiding (lights)
import qualified Graphics.Vty as G
import           Lens.Micro
import           Lens.Micro.Platform ()
import           Text.Printf.TH

---

-- | The application event handler. Resizing still happens automatically.
handle :: System -> BrickEvent t EnConEvent -> EventM RName (Next System)
handle s (AppEvent NextTrack) = castTopTrack s
handle s (VtyEvent (G.EvKey G.KEsc _)) = halt s
handle s (VtyEvent (G.EvKey G.KLeft [G.MShift])) = continue (s & msg .~ "<==" & pages %~ D.shiftRight)
handle s (VtyEvent (G.EvKey G.KRight [G.MShift])) = continue (s & msg .~ "==>" & pages %~ D.shiftLeft)
handle s e = case fromJust . D.head $ _pages s of
  Lights -> lightH s e
  Media  -> mediaH s e
  Logs   -> logH s e

-- | Handle events unique to the Lights page.
lightH :: System -> BrickEvent t t1 -> EventM RName (Next System)
lightH s (VtyEvent e) = case listSelectedElement $ _lightGroups s of
  Nothing -> continue s
  Just (_, (i, g)) -> case e of
    G.EvKey G.KEnter _ -> do
      liftIO . runEffect (_env s) $ overGroup lightOn i
      continue (s & msg .~ [st|ON: %s|] (_gname g)
                  & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOn))
    G.EvKey G.KBS _ -> do
      liftIO . runEffect (_env s) $ overGroup lightOff i
      continue (s & msg .~ [st|OFF: %s|] (_gname g)
                  & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightOff))
    G.EvKey (G.KChar '0') _ -> do
      liftIO . runEffect (_env s) $ overGroup (lightBri 1) i
      continue (s & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightBri 1))
    G.EvKey (G.KChar c) _ | c `elem` ['1'..'9'] -> do
      let n = read [c] / 10
      liftIO . runEffect (_env s) $ overGroup (lightBri n) i
      continue (s & lightGroups %~ listModify (\e -> e & _2 . gaction %~ lightBri n))
                          | otherwise -> handleEventLensed s lightGroups handleListEvent e >>= continue
    _ -> handleEventLensed s lightGroups handleListEvent e >>= continue
lightH s _ = continue s

-- | Handle events unique to the Media page.
mediaH :: System -> BrickEvent t EnConEvent -> EventM RName (Next System)
mediaH s (VtyEvent (G.EvKey G.KEnter _))       = castTopTrack s
mediaH s (VtyEvent (G.EvKey (G.KChar 'p') _))  = liftIO (runEffect (_env s) pause) >> continue (s & msg .~ "Pausing ChromeCast.")
mediaH s (VtyEvent (G.EvKey (G.KChar 'c') _))  = liftIO (runEffect (_env s) unpause) >> continue (s & msg .~ "Unpausing ChromeCast.")
mediaH s (VtyEvent (G.EvKey (G.KChar 's') _))  = stopCast s
mediaH s (VtyEvent (G.EvKey G.KBS _))          = continue (s & playlist %~ listClear)
mediaH s (VtyEvent (G.EvKey (G.KChar '\t') _)) = continue $ tabH s
mediaH s (VtyEvent (G.EvKey (G.KChar ' ') _))  = continue $ spaceH s
mediaH s (VtyEvent e) | _trackView s = handleEventLensed s albumTracks handleListEvent e >>= continue
                      | otherwise = handleEventLensed s mediaFiles handleListEvent e >>= continue

-- | Spawn a thread that casts the top track in the playlist, waiting for it
-- to finish. When it does, it yields a custom event to the event loop that
-- causes this to be called again.
castTopTrack :: System -> EventM n (Next System)
castTopTrack s | null (_playlist s) = continue (s & msg .~ "No tracks in the playlist.")
               | otherwise = do
                   liftIO . maybe (pure ()) cancel $ _castThread s
                   let track = _playlist s ^?! listElementsL . _head
                   a <- liftIO . async $ castWork s track
                   continue (s & msg .~ [st|Casting %s|] (displayName track)
                               & castThread .~ Just a
                               & playlist %~ listRemove 0)

-- | Cast a track, and only submit a `NextTrack` event if the casting succeeded.
castWork :: System -> T.Text -> IO ()
castWork s track = do
  res <- runEffect (_env s) $ cast' track
  either (\_ -> pure ()) (\_ -> writeBChan (_eventChan s) NextTrack) res

stopCast :: System -> EventM n (Next System)
stopCast s = case _castThread s of
  Nothing -> continue (s & msg .~ "No casting thread to kill.")
  Just a  -> do
    liftIO $ cancel a >> runEffect (_env s) stop
    continue (s & castThread .~ Nothing & msg .~ "Stopping ChromeCast.")

-- | Decide where to move the cursor focus.
tabH :: System -> System
tabH s | _trackView s = s & trackView .~ False & albumTracks %~ listClear
       | otherwise = pushAlbumTracks s

-- | Add a track or album to the playlist.
spaceH :: System -> System
spaceH s | _trackView s = case listSelectedElement $ _albumTracks s of
             Nothing -> s
             Just (_,i) -> s & playlist %~ (\l -> listInsert (length l) i l)
                             & albumTracks %~ listMoveDown
                             & msg .~ [st|Adding %s to playlist.|] (displayName i)
         | otherwise = case listSelectedElement $ _mediaFiles s of
               Nothing -> s
               Just (_, Video t) -> s & playlist %~ (\l -> listInsert (length l) t l)
               Just (_, Album _ ts) -> s & playlist .~ list Playlist (V.fromList $ toList (_playlist s) <> ts) 1

-- | Display a TAB'd Album's tracks in the tracks list.
pushAlbumTracks :: System -> System
pushAlbumTracks s = case listSelectedElement $ _mediaFiles s of
  Nothing -> s
  Just (_, Video _) -> s & msg .~ "Can't expand - that's a video file."
  Just (_, Album t ts) -> s & albumTracks %~ listReplace (V.fromList ts) (Just 0)
                            & trackView .~ True
                            & msg .~ "Displaying tracks for: " <> t

-- | Handle events unique to the Log page.
logH :: System -> BrickEvent t t1 -> EventM RName (Next System)
logH s (VtyEvent e) = handleEventLensed s logEntries handleListEvent e >>= continue
