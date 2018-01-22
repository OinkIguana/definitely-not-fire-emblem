{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.Menu where
  import Control.Monad
  import qualified SDL
  import qualified SDL.Font as Font
  import Foreign.C.Types
  import Lib.Resources.Fonts
  import Lib.Model.Game
  import Lib.RC

  renderMenuQuick :: Menu -> StateRC SDL.Texture
  renderMenuQuick Menu { options } = do
    let color = SDL.V4 255 255 255 255
        len = length options
    font <- getFont fontDefault
    sep <- Font.lineSkip font
    renderer <- getRenderer
    width <- maximum . map fst <$> mapM (Font.size font . fst) options
    surfaces <- mapM (Font.solid font color . fst) options
    combined <- SDL.createRGBSurface (SDL.V2 (fromIntegral width) (fromIntegral $ sep * len)) SDL.RGBA8888
    let
      alignOption :: Int -> SDL.Surface -> StateRC (Maybe (SDL.Rectangle CInt))
      alignOption i surface =
        SDL.surfaceBlit surface Nothing combined $
          Just $ toSDL $ Point (0 :: CInt) (fromIntegral $ i * sep)
    mapM_ (uncurry alignOption) (zip [0..] surfaces)
    SDL.createTextureFromSurface renderer combined
