{-# LANGUAGE OverloadedStrings #-}
module Lib.Model.MainMenu (new) where
  import Data.Text
  import Lib.Model.Game
  import qualified Lib.Model.Menu as Menu
  import qualified Lib.Model.SettingsMenu as SettingsMenu
  import qualified Lib.Action.MainMenu as MainMenu
  import qualified Lib.Action.Game as Game

  new :: Room
  new = MainMenu $ Menu.new
      [ (const "New Game", return)
      , (const "Continue", return)
      , (const "Multiplayer", return)
      , (const "Settings", MainMenu.addSubmenu SettingsMenu.new)
      , (const "Quit", Game.quit) ]
