module Lib.Constants where
  import Lib.Model
  import Foreign.C.Types

  tileSize :: Dimension CInt
  tileSize = Dimension 64 64
