module Lib.Model.Board.Simple where
  import Data.Grid
  import Lib.Model

  emptyPlains :: Board
  emptyPlains = Board $ squareGrid 8 (Tile Plain Nothing)
