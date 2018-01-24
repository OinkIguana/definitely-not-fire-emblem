{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Action.Menu
  ( nextOption
  , previousOption
  , selectOption
  ) where
  import Lib.Model.Game
  import Data.Maybe

  nextOption :: Menu -> Menu
  nextOption = nextOption_ . unnestMenu
    where
      nextOption_ Menu { options, selection, .. } =
        Menu { selection = (selection + 1) `mod` length options, .. }

  previousOption :: Menu -> Menu
  previousOption = previousOption_ . unnestMenu
    where
      previousOption_ Menu { options, selection, .. } =
        let len = length options in
          Menu { selection = (selection - 1 + len) `mod` len, .. }

  selectOption :: Menu -> Action
  selectOption = selectOption_ . unnestMenu
    where
      selectOption_ Menu { options, selection, .. } =
        snd $ options !! selection

  unnestMenu :: Menu -> Menu
  unnestMenu menu = fromMaybe menu $ submenu menu
