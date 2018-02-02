{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.MainMenu (view) where
  import Data.Shape
  import SDL hiding (Rectangle)
  import Lib.RC
  import Lib.Model
  import Lib.View.Menu

  view :: Game -> Menu -> StateRC ()
  view game menu = do
    renderer <- getRenderer
    menuTexture <- renderMenuQuick game menu
    TextureInfo { textureWidth, textureHeight } <- queryTexture menuTexture
    copy renderer menuTexture Nothing (Just $ toSDL $ Rectangle 0 0 textureWidth textureHeight)
    destroyTexture menuTexture
