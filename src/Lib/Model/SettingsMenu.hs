{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Lib.Model.SettingsMenu (new) where
  import Data.Text
  import Lib.Model.Game
  import qualified Lib.Model.Menu as Menu
  import qualified Lib.Action.SettingsMenu as SettingsMenu
  import qualified Lib.Action.MainMenu as MainMenu

  new :: Menu
  new = Menu.new
    [ (const "Back", MainMenu.back)
    , (autoEndLabel, SettingsMenu.toggleAutoEnd)
    , (combatAnimationsLabel, SettingsMenu.toggleCombatAnimations)
    , (movementAnimationsLabel, SettingsMenu.toggleMovementAnimations)
    ]

  autoEndLabel :: Game -> Text
  autoEndLabel Game { settings = Settings { autoEnd } } =
    "Auto end: " ++ if autoEnd then "Enabled" else "Disabled"

  combatAnimationsLabel :: Game -> Text
  combatAnimationsLabel Game { settings = Settings { combatAnimations } } =
    "Combat animations: " ++ if combatAnimations then "Enabled" else "Disabled"

  movementAnimationsLabel :: Game -> Text
  movementAnimationsLabel Game { settings = Settings { movementAnimations } } =
    "Movement animations: " ++ if movementAnimations then "Enabled" else "Disabled"
