{-# LANGUAGE OverloadedStrings #-}
module Lib.Model.MainMenu (new) where
  import Data.Text
  import Lib.Model.Game
  import qualified Lib.Model.Menu as Menu
  import qualified Lib.Action.Game as Game

  new :: MainMenu
  new = Menu.new
      [ ("New Game", return)
      , ("Continue", return)
      , ("Multiplayer", return)
      , ("Settings", return)
      , ("Quit", Game.quit) ]
