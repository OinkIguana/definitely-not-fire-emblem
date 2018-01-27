module Lib.Model.Game (new) where
  import Data.Text
  import Lib.Model
  import qualified Lib.Model.MainMenu as MainMenu
  import qualified Lib.Model.SaveData as SaveData

  new :: Game
  new = Game defaultSettings defaultEnvironment SaveData.new MainMenu.new False

  defaultSettings :: Settings
  defaultSettings = Settings True True True False

  defaultEnvironment :: Environment
  defaultEnvironment = Environment (Point 0 0) ()
