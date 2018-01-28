{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.PauseMenu (view) where
  import SDL hiding (Rectangle)
  import Lib.RC
  import Lib.Model
  import Lib.View.Menu
  import Foreign.C.Types

  view :: Game -> Menu -> StateRC ()
  view game menu = do
    renderer <- getRenderer
    menuTexture <- renderMenuQuick game menu
    Display { displayBoundsSize = V2 screenWidth screenHeight } <- head <$> getDisplays
    TextureInfo { textureWidth, textureHeight } <- queryTexture menuTexture
    let
      menuArea = Rectangle ((screenWidth - textureWidth) `div` 2) ((screenHeight - textureHeight) `div` 2) textureWidth textureHeight
      sdlMenuArea = Just $ toSDL menuArea
    backgroundTexture <- getTexture (background menuArea)
    copy renderer backgroundTexture Nothing sdlMenuArea
    copy renderer menuTexture Nothing sdlMenuArea
    destroyTexture menuTexture

  background :: Rectangle CInt -> Key Texture
  background box = keyFor "Lib.View.PauseMenu.background" $ do
    renderer <- getRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessTarget $ toSDL $ dimensions box
    rendererRenderTarget renderer $= Just texture
    rendererDrawColor renderer $= V4 60 60 60 255
    fillRect renderer Nothing
    rendererRenderTarget renderer $= Nothing
    return texture
