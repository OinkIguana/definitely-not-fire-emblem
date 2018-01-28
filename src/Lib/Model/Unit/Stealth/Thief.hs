{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Lib.Model.Unit.Stealth.Thief where
  import SDL hiding (Rectangle)
  import Lib.Model
  import Lib.RC
  import Foreign.C.Types

  baseStats :: Stats
  baseStats = Stats
    { mhp = 18
    , chp = 18
    , atk = 7
    , def = 7
    , spd = 9
    , lck = 16
    , skl = 18
    , mov = 5
    , snk = 33
    , vis = 3
    }

  basic :: Unit
  basic = Unit Thief "Thief" baseStats [Knife] [Steal] (Sprite (Base $ Static texture $ Rectangle 0 0 32 32))

  texture :: Key Texture
  texture = keyFor "Lib.Model.Unit.Stealth.Thief" $ do
    renderer <- getRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessTarget $ toSDL $ Dimension (32 :: CInt) 32
    rendererRenderTarget renderer $= Just texture
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRect renderer Nothing
    rendererRenderTarget renderer $= Nothing
    return texture
