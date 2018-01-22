module Lib.View.MainMenu (view) where
  import SDL
  import Lib.RC
  import Lib.Model.Game
  import Lib.View.Menu

  view :: Menu -> StateRC ()
  view menu = do
    renderer <- getRenderer
    texture <- renderMenuQuick menu
    copy renderer texture Nothing Nothing
