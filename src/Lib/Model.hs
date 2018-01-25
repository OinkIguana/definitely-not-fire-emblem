module Lib.Model where
  import Data.Text
  import Lib.Model.Game
  import Lib.Model.MainMenu as MainMenu

  newGame :: Game
  newGame = Game defaultSettings [] MainMenu.new False

  defaultSettings :: Settings
  defaultSettings = Settings True True True
