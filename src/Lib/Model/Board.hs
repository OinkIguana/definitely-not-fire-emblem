module Lib.Model.Board where
  import Data.Grid
  import Lib.Model

  addUnit :: Int -> Int -> Unit -> Board -> Board
  addUnit x y unit (Board grid) =
    Board $ updateAt x y (\tile -> tile { unit = Just unit }) grid
