{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Lib.Action.Menu
  ( nextOption
  , previousOption
  , selectOption
  , submenu
  , back
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

  addSubmenu :: Menu -> Menu -> Menu
  addSubmenu toAdd Menu { submenu = Nothing, .. } = Menu { submenu = toAdd, .. }
  addSubmenu toAdd menu = menu { submenu = addSubmenu toAdd $ submenu menu }

  back :: Menu -> Menu
  back menu =
    case submenu menu of
      Nothing -> menu
      Just menu' -> case submenu menu' of
        Nothing -> menu'
        Just menu'' = back menu'

  unnestMenu :: Menu -> Menu
  unnestMenu menu = fromMaybe menu $ submenu menu
