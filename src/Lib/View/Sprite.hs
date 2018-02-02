{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.Sprite (view) where
  import Data.Shape
  import SDL hiding (Point, Rectangle)
  import Lib.Model
  import Lib.RC
  import Foreign.C.Types (CInt)

  view :: Sprite -> StateRC ()
  view (Sprite sprite) = spriteEffect (Point 0 0) (Dimension 1 1) sprite

  spriteEffect :: Point CInt -> Dimension CInt -> SpriteEffect -> StateRC ()
  spriteEffect dest scale (Base sprite)          = spriteBase dest scale sprite
  spriteEffect _    scale (Position dest sprite) = spriteEffect dest scale sprite
  spriteEffect dest _     (Scale scale sprite)   = spriteEffect dest scale sprite

  spriteBase :: Point CInt -> Dimension CInt -> SpriteBase -> StateRC ()
  spriteBase (Point x y) (Dimension sx sy) (Static key src) = do
    renderer <- getRenderer
    texture <- getTexture key
    TextureInfo { textureWidth, textureHeight } <- queryTexture texture
    let area = Rectangle x y (textureWidth * sx) (textureHeight * sy)
    copy renderer texture (Just $ toSDL src) (Just $ toSDL area)
    return ()
  spriteBase _ _ _ = return ()
