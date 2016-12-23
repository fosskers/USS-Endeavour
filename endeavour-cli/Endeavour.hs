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
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Lens.Micro
import           Lens.Micro.TH
import           System.Posix.User (getEffectiveUserName)
import           Text.Printf.TH

---

-- | The application state.
data System = System { _msg :: T.Text } deriving (Eq, Show)
makeLenses ''System

-- | All resource names.
data Name = Jack deriving (Eq, Show, Ord)

boxy :: Widget n
boxy = withBorderStyle unicodeRounded . border . padAll 5 $ txt "Welcome, Officer."

widgets :: System -> Widget n
widgets s = vBox [ centerWith (Just '.') boxy
                 , padRight Max . padLeft (Pad 1) . txt $ _msg s --progressBar (Just "Progress") 0.4
                 ]

-- | The final conglomeration of `Widget`s.
ui :: System -> Widget n
ui s = withBorderStyle unicodeBold . borderWithLabel (padLeftRight 1 $ txt "Endeavour System Controls") $ widgets s

-- | A description of how to run our application.
app :: App System () Name
app = App { appDraw = \s -> [ui s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = resizeOrQuit
          , appStartEvent = pure
          , appAttrMap = const $ attrMap V.defAttr [ (progressCompleteAttr, bg V.blue) ]
          }

main :: IO ()
main = do
  user <- T.pack <$> getEffectiveUserName
  void . defaultMain app . System $ [st|Hello, %s.|] (T.toTitle user)
