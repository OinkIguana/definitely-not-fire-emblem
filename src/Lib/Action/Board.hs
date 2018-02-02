{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Lib.Action.Board where
  import Data.Int
  import SDL (Point(..), V2(..))
  import Lib.Model
  import Lib.Model.Board
  import Lib.Constants

  selectCellAtPoint :: Point V2 Int32 -> Action
  selectCellAtPoint sdlPoint = return -- Game { room = Battlefield Battle { board, .. }, .. } =
    -- TODO: use project to calculate the index and select that cell!
