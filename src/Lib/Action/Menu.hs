{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildcards #-}
module Lib.Action.Menu
  ( nextOption
  , previousOption
  , selectOption
  ) where
  import Lib.Model.Game

  nextOption :: Action
  nextOption = nextOption_ . unnestMenu
    where nextOption Menu { options, selected, .. } =
      Menu { selected = (selected + 1) % length options, .. }

  previousOption :: Action
  previousOption = nextOption . unnestMenu
    where previousOption_ Menu { options, selected, .. } =
      let len = length options in Menu { selected = (selected - 1 + len) % len, .. }

  selectOption :: Menu -> Action
  selectOption = selectOption_ . unnestMenu
    where selectOption_ Menu { options, selected, .. } =
      snd $ options !! selected

  unnestMenu :: Menu -> Menu
  unnestMenu menu = fromMaybe menu $ menu >>= submenu
