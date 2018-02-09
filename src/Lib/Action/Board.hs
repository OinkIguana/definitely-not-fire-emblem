module Lib.Action.Board (selectCellAtPoint) where
  import Prelude.Unicode
  import Data.Shape
  import Data.Grid
  import Data.Int
  import qualified Data.Set as Set
  import Control.Monad
  import qualified SDL (Point, V2)
  import Lib.Model
  import Lib.Model.Board hiding (indexOf)
  import Lib.Constants

  selectCellAtPoint ∷ SDL.Point SDL.V2 Int32 → Action
  selectCellAtPoint sdlPoint Game { room = Battlefield Battle { board = Board { grid }, .. }, .. }
    = let Dimension w h = tileSize
          Rectangle x y _ _ = project (Dimension (fromIntegral w) (fromIntegral h)) (Rectangle 0 0 1 1) (fromSDL sdlPoint :: Point Float)
          newBoard = Board $ maybe grid (flip selectedAt grid) $ indexOf (floor x) (floor y) grid
      in return Game { room = Battlefield Battle { board = newBoard, .. }, .. }


  selectedAt ∷ Int → Grid Tile → Grid Tile
  selectedAt index grid =
    grid { cells = fmap (uncurry $ selectAt index) indexedGrid }
      where
        indexedGrid = zip [0..] $ cells grid
        selectAt ∷ Int → Int → Tile → Tile
        selectAt target point tile =
          if target == point
            then tile { highlight = Set.insert Select $ highlight tile }
            else tile { highlight = Set.delete Select $ highlight tile }
