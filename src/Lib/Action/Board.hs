module Lib.Action.Board where
  import
  import Control.Monad
  import Data.Int
  import SDL (Point(..), V2(..))
  import Lib.Model
  import Lib.Model.Board
  import Lib.Constants

  selectCellAtPoint ∷ Point V2 Int32 → Action
  selectCellAtPoint sdlPoint Game { room = Battlefield Battle { board = { grid }, .. }, .. }
    = Game { room = Battlefield Battle { board = selectedAt (Point x y) board, .. }, .. }
      where Rectangle x y _ _ = project (vectorRectangle (Point 0 0) tileSize)
                                        (Rectangle 0 0 1 1)
                                        (vectorRectangle (fmap fromInteger $ fromSDL sdlPoint) tileSize)


  selectedAt ∷ Point a → Grid Tile → Grid Tile
  selectedAt point grid =
    fmap (uncurry $ selectAt point) indexedGrid
      where
        indexedGrid = zipWithM (,) (fmap (positionIn grid) [0..]) grid
        selectAt ∷ Point a → Point a → Tile → Tile
        selectAt target point tile =
          if target == point
            then tile { highlight = insert Selected $ highlight tile }
            else tile { highlight = delete Selected $ highlight tile }
