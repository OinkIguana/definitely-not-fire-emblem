module Lib.Resources.Fonts where
  import SDL.Font
  import Lib.RC
  import Control.Monad.State

  fontDefault :: Key Font
  fontDefault = keyFor "Lib.Resources.Fonts.fontDefault" (liftIO $ load "resources/fonts/Roboto-Regular.ttf" 15)
