{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import           Brick
import           Brick.Widgets.List
import           Control.Monad (void)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Deque as D
import           Endeavour.Console.Events
import           Endeavour.Console.Types
import           Endeavour.Console.UI
import           Endeavour.Genetics
import           Endeavour.Knowledge.ChromeCast
import           Endeavour.Knowledge.Hue hiding (lights)
import           Endeavour.Knowledge.Space
import           Endeavour.Memory
import qualified Graphics.Vty as G
import           Options.Generic
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | Command-line arguments.
newtype Args = Args { config :: FilePath } deriving (Generic, ParseRecord)

-- | A description of how to run our application.
app :: App System () RName
app = App { appDraw = \s -> [ui s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handle
          , appStartEvent = pure
          , appAttrMap = const $ attrMap G.defAttr [ (selected, bg G.blue)
                                                   , (logWarn, bg G.yellow)
                                                   , (logFail, bg G.red)
                                                   , (onAttr, bg G.yellow)
                                                   , (offAttr, bg G.red)
                                                   , (albumAttr, G.withStyle G.defAttr G.underline)
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
      mdia <- runM $ runReader ((++) <$> albums <*> videos) e
      logs <- runM $ runReader (recall Nothing) e
      user <- T.pack <$> getEffectiveUserName
      astr <- either (const 0) length <$> runEffect e astronauts
      let sys = System
            { _env = e
            , _msg = [st|Hello, %s. There are currently %d humans in space.|] (T.toTitle user) astr
            , _pages = D.fromList [Lights ..]
            , _lightGroups = list LGroupList (V.fromList grps) 1
            , _mediaFiles  = list MediaList (V.fromList mdia) 1
            , _albumTracks = list AlbumTracks (V.fromList []) 1
            , _playlist    = list Playlist (V.fromList []) 1
            , _trackView   = False
            , _logEntries  = list LogList (V.fromList logs) 1 }
      void $ defaultMain app sys
      slumber e
      putStrLn "Shutdown complete."
