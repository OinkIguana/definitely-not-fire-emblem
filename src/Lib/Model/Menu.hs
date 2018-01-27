module Lib.Model.Menu (new) where
  import Data.Text
  import Lib.Model

  new :: [(Game -> Text, Action)] -> Menu
  new options = Menu options 0 Nothing
