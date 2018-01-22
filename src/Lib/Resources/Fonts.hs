module Lib.Resources.Fonts where
  import SDL.Font
  import Lib.RC

  fontDefault :: Key Font
  fontDefault = keyFor "Lib.Resources.Fonts.fontDefault" (load "resources/fonts/Roboto-Regular.ttf" 15)
