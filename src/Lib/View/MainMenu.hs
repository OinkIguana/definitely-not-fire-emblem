{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.MainMenu (view) where
  import SDL (copy, queryTexture, TextureInfo(..), destroyTexture)
  import Lib.RC
  import Lib.Model.Game
  import Lib.View.Menu

  view :: Menu -> StateRC ()
  view menu = do
    renderer <- getRenderer
    texture <- renderMenuQuick menu
    TextureInfo { textureWidth, textureHeight } <- queryTexture texture
    copy renderer texture Nothing (Just $ toSDL $ Rectangle 0 0 textureWidth textureHeight)
    destroyTexture texture
