module Lib.View (view) where
  import qualified Lib.View.MainMenu as MainMenu
  import Lib.Model.Game
  import Lib.RC

  view :: Game -> StateRC ()
  view (Game _ _ (MainMenu menu)) = MainMenu.view menu
