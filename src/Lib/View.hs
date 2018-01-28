module Lib.View (view) where
  import qualified Lib.View.Room as Room
  import Lib.Model
  import Lib.RC

  view :: Game -> StateRC ()
  view game = Room.view game (room game)
