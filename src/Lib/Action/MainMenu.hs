{-# LANGUAGE RecordWildcards #-}
module Lib.Action.MainMenu where
  import qualified Lib.Action.Menu as Menu
  import Lib.Model.Game

  previousOption :: Action
  nextOption = withMainMenu Menu.nextOption

  previousOption :: Action
  previousOption = withMainMenu Menu.previousOption

  selectOption :: Action
  selectOption game = maybe return identity (takeMainMenu game >>= Menu.selectOption) game

  withMainMenu :: (Menu -> Menu) -> Action
  withMainMenu f Game { room = MainMenu menu, .. } = return Game { room = MainMenu $ f menu, .. }
  withMainMenu _ game = return game

  takeMainMenu :: Game -> Maybe Menu
  takeMainMenu Game { room = MainMenu menu } = Just menu
  takeMainMenu _ = Nothing
