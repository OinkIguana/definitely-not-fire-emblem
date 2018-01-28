{-# LANGUAGE OverloadedStrings #-}
module Lib.Model.PauseMenu where
  import Lib.Model
  import Data.Text
  import qualified Lib.Model.Menu as Menu
  import qualified Lib.Action.PauseMenu as PauseMenu

  new :: Room -> Room
  new = PauseMenu pauseMenu

  pauseMenu :: Menu
  pauseMenu = Menu.new
    [ (const "Resume", PauseMenu.unpause)
    , (const "Quit", PauseMenu.quit) ]
