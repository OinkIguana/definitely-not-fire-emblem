module Lib.Model.Sprite where
  import Data.Shape
  import Lib.Model
  import Foreign.C.Types (CInt)

  positioned :: Point CInt -> Sprite -> Sprite
  positioned point (Sprite sprite) = Sprite $ Position point sprite
