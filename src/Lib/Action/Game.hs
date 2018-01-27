module Lib.Action.Game (quit, navigate) where
  import qualified Lib.Model as Model

  quit :: Model.Action
  quit game = return game { Model.quit = True }

  navigate :: Model.Room -> Model.Action
  navigate newRoom game =
    return game { Model.room = newRoom }
