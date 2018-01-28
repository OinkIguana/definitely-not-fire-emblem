{-# LANGUAGE NamedFieldPuns #-}
module Lib.View.Unit (view, viewAtTile) where
  import SDL hiding (Point)
  import Lib.Model
  import Lib.RC
  import Lib.Model.Sprite
  import qualified Lib.View.Sprite as Sprite
  import Foreign.C.Types (CInt)

  view :: Unit -> StateRC ()
  view Unit { sprite } = Sprite.view sprite

  viewAtTile :: Point CInt -> Unit -> StateRC ()
  viewAtTile point Unit { sprite } = Sprite.view $ positioned point sprite
