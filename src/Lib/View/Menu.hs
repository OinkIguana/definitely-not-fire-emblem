{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.Menu (renderMenuQuick) where
  import Control.Monad
  import Data.Text (Text)
  import qualified SDL
  import qualified SDL.Font as Font
  import Foreign.C.Types
  import Lib.Resources.Fonts
  import Lib.Model
  import Lib.RC

  renderMenuQuick :: Game -> Menu -> StateRC SDL.Texture
  renderMenuQuick game Menu { options, selection } = do
    let
      len = length options
      optionTexts = map (($ game) . fst) options
    font <- getFont fontDefault
    sep <- Font.lineSkip font
    renderer <- getRenderer
    width <- maximum . map fst <$> mapM (Font.size font) optionTexts
    surfaces <- sequence $ renderOptions font selection optionTexts
    combined <- SDL.createRGBSurface (SDL.V2 (fromIntegral width) (fromIntegral $ sep * len)) SDL.RGBA8888
    let
      alignOption :: Int -> SDL.Surface -> StateRC (Maybe (SDL.Rectangle CInt))
      alignOption i surface =
        SDL.surfaceBlit surface Nothing combined $
          Just $ toSDL $ Point (0 :: CInt) (fromIntegral $ i * sep)
    mapM_ (uncurry alignOption) (zip [0..] surfaces)
    mapM_ SDL.freeSurface surfaces
    SDL.createTextureFromSurface renderer combined

  renderOptions :: Font.Font -> Int -> [Text] -> [StateRC SDL.Surface]
  renderOptions font 0 (option : options) = renderSelectedOption font option : renderOptions font (-1) options
  renderOptions font n (option : options) = renderOption font option : renderOptions font (n - 1) options
  renderOptions _ _ [] = []

  renderOption :: Font.Font -> Text -> StateRC SDL.Surface
  renderOption font = Font.solid font white

  renderSelectedOption :: Font.Font -> Text -> StateRC SDL.Surface
  renderSelectedOption font = Font.solid font red

  white = SDL.V4 255 255 255 255
  red = SDL.V4 255 0 0 0
