module Lib.View.Room (view) where
  import qualified Lib.View.MainMenu as MainMenu
  import qualified Lib.View.PauseMenu as PauseMenu
  import qualified Lib.View.Battlefield as Battlefield
  import Lib.Model
  import Lib.RC

  view :: Game -> Room -> StateRC ()
  view game room =
    case room of
      MainMenu menu -> MainMenu.view game menu
      Battlefield battle -> Battlefield.view game battle
      -- NOTE: could save subroom in a texture for efficiency, but this would prevent animations if
      --       those are desired
      PauseMenu menu pausedRoom -> view game pausedRoom >> PauseMenu.view game menu
      _ -> return ()
