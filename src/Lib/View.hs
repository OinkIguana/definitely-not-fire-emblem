module Lib.View (view) where
  import qualified Lib.View.MainMenu as MainMenu
  import qualified Lib.View.Battlefield as Battlefield
  import Lib.Model
  import Lib.RC

  view :: Game -> StateRC ()
  view game =
    case game of
      Game { room = MainMenu menu } -> MainMenu.view game menu
      Game { room = Battlefield battle } -> Battlefield.view game battle
      _ -> return ()
