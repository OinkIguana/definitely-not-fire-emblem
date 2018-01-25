module Lib.Model where
  import Data.Text
  import Lib.Model.Game
  import Lib.Model.MainMenu as MainMenu

  newGame :: Game
  newGame = Game defaultSettings defaultEnvironment defaultSaveData MainMenu.new False

  defaultSettings :: Settings
  defaultSettings = Settings True True True

  defaultEnvironment :: Environment
  defaultEnvironment = Environment (Point 0 0)

  defaultSaveData :: SaveData
  defaultSaveData = SaveData 0 []
