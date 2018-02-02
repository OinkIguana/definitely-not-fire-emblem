module Lib.Model.Board where
  import Data.Grid
  import Lib.Model

  addUnit :: Int -> Int -> Unit -> Board -> Board
  addUnit x y unit board =
    board { grid = updateAt x y (\tile -> tile { unit = Just unit }) (grid board) }
