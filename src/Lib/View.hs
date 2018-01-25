module Lib.View (view) where
  import qualified Lib.View.MainMenu as MainMenu
  import Lib.Model.Game
  import Lib.RC

  view :: Game -> StateRC ()
  view game =
    case game of
      Game { room = MainMenu menu } -> MainMenu.view game menu
      _ -> return
