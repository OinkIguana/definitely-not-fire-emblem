module Lib.Model.Stage.Tutorial where
  import Lib.Model
  import Lib.Model.Board
  import Lib.Model.Board.Simple
  import qualified Lib.Model.Unit.Stealth.Thief as Thief

  tutorial :: Room
  tutorial = Battlefield $ Battle [] board 0
    where board = addUnit 0 0 Thief.basic
                $ addUnit 1 1 Thief.basic
                $ addUnit 2 2 Thief.basic
                $ emptyPlains
