module Lib.Model.Board where
  import qualified Data.Grid as Grid
  import Lib.Model

  addUnit :: Int -> Int -> Unit -> Board -> Board
  addUnit x y unit board =
    board { grid = Grid.updateAt x y (\tile -> tile { unit = Just unit }) (grid board) }

  width :: Board -> Int
  width Board { grid } = Grid.width grid

  height :: Board -> Int
  height Board { grid } = Grid.height grid

  indexOf :: Int -> Int -> Board -> Maybe Int
  indexOf x y Board { grid } = Grid.indexOf x y grid
