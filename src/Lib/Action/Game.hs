module Lib.Action.Game
  ( quit
  , navigate
  , pause
  ) where
  import qualified Lib.Model as Model
  import qualified Lib.Model.PauseMenu as PauseMenu

  quit :: Model.Action
  quit game = return game { Model.quit = True }

  navigate :: Model.Room -> Model.Action
  navigate newRoom game = return game { Model.room = newRoom }

  pause :: Model.Action
  pause game = return game { Model.room = PauseMenu.new (Model.room game) }
