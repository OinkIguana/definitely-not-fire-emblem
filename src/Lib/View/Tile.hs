{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.Tile (view) where
  import SDL hiding (Point)
  import Lib.Model
  import Lib.RC
  import Lib.Constants

  view :: Game -> Point Int -> Tile -> StateRC ()
  view _ position Tile { terrain } = do
    texture <- getTexture (textureFor terrain)
    renderer <- getRenderer
    copy renderer texture Nothing $ Just $ toSDL $ vectorRect scaledPosition tileSize
    return ()
      where Point x y = position
            Dimension w h = tileSize
            scaledPosition = Point (fromIntegral x * w) (fromIntegral y * h)

  textureFor :: Terrain -> Key SDL.Texture
  textureFor Plain = keyFor "Lib.View.Tile.plain" plain
  textureFor _     = undefined

  plain :: StateRC Texture
  plain = do
    renderer <- getRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessTarget $ toSDL tileSize
    rendererRenderTarget renderer $= Just texture
    rendererDrawColor renderer $= V4 0 255 0 255
    fillRect renderer Nothing
    rendererDrawColor renderer $= V4 0 0 0 255
    drawRect renderer Nothing
    rendererRenderTarget renderer $= Nothing
    return texture
