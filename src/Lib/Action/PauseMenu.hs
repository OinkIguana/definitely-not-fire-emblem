module Lib.Action.PauseMenu
  ( nextOption
  , previousOption
  , selectOption
  , addSubmenu
  , back
  , unpause
  , Lib.Action.PauseMenu.quit
  ) where
  import Lib.Model
  import qualified Lib.Action.Menu as Menu

  nextOption :: Action
  nextOption = withPauseMenu Menu.nextOption

  previousOption :: Action
  previousOption = withPauseMenu Menu.previousOption

  selectOption :: Action
  selectOption game =
    maybe return Menu.selectOption (takePauseMenu game) game

  addSubmenu :: Menu -> Action
  addSubmenu = withPauseMenu . Menu.addSubmenu

  back :: Action
  back = withPauseMenu Menu.back

  withPauseMenu :: (Menu -> Menu) -> Action
  withPauseMenu f Game { room = PauseMenu menu pausedRoom, .. } = return Game { room = PauseMenu (f menu) pausedRoom, .. }
  withPauseMenu _ game = return game

  takePauseMenu :: Game -> Maybe Menu
  takePauseMenu Game { room = PauseMenu menu pausedRoom } = Just menu
  takePauseMenu _ = Nothing

  unpause :: Action
  unpause game = case room game of
    PauseMenu _ pausedRoom -> return game { room = pausedRoom }
    otherwise              -> return game

  -- Why is a duplicate of Lib.Action.Game.quit?
  -- To avoid circular dependencies of course...
  quit :: Action
  quit game = return game { Lib.Model.quit = True }
