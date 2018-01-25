module Lib.Model.Menu (new) where
  import Data.Text
  import Lib.Model.Game

  new :: [(Text, Action)] -> Menu
  new options = Menu options 0 Nothing
