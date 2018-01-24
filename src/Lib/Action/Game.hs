module Lib.Action.Game (quit) where
  import qualified Lib.Model.Game as Game

  quit :: Game.Action
  quit game = return game { Game.quit = True }
