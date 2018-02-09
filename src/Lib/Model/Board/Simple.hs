module Lib.Model.Board.Simple where
  import Data.Grid
  import Lib.Model
  import qualified Data.Set as Set

  emptyPlains :: Board
  emptyPlains = Board (squareGrid 8 $ Tile Plain Nothing Set.empty)
