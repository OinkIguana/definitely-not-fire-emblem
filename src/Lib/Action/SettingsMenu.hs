{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Lib.Action.SettingsMenu where
  import Lib.Model

  toggleAutoEnd :: Action
  toggleAutoEnd Game { settings, .. } = return Game { settings = settings { autoEnd = not $ autoEnd settings } , .. }

  toggleMovementAnimations :: Action
  toggleMovementAnimations Game { settings, .. } = return Game { settings = settings { movementAnimations = not $ movementAnimations settings } , .. }

  toggleCombatAnimations :: Action
  toggleCombatAnimations Game { settings, .. } = return Game { settings = settings { combatAnimations = not $ combatAnimations settings } , .. }
