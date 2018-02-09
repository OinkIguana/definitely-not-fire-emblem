module Lib.Action.Menu
  ( nextOption
  , previousOption
  , selectOption
  , addSubmenu
  , back
  ) where
  import Lib.Model
  import Data.Maybe

  nextOption :: Menu -> Menu
  nextOption = nextOption_
    where
      nextOption_ Menu { options, selection, .. } =
        Menu { selection = (selection + 1) `mod` length options, .. }

  previousOption :: Menu -> Menu
  previousOption = previousOption_
    where
      previousOption_ Menu { options, selection, .. } =
        let len = length options in
          Menu { selection = (selection - 1 + len) `mod` len, .. }

  selectOption :: Menu -> Action
  selectOption = selectOption_
    where
      selectOption_ Menu { options, selection, .. } =
        snd $ options !! selection

  addSubmenu :: Menu -> Menu -> Menu
  addSubmenu toAdd menu = toAdd { submenu = Just menu }

  back :: Menu -> Menu
  back Menu { submenu = Just menu } = menu
