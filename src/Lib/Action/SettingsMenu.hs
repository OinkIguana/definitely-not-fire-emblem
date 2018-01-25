{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Lib.Action.SettingsMenu where
  import Lib.Model.Game

  toggleAutoEnd :: Action
  toggleAutoEnd Game { settings, .. } = Game { settings = settings { autoEnd = not $ autoEnd settings } , .. }

  toggleMovementAnimations :: Action
  toggleMovementAnimations Game { settings, .. } = Game { settings = settings { movementAnimations = not $ movementAnimations settings } , .. }

  toggleCombatAnimations :: Action
  toggleCombatAnimations Game { settings, .. } = Game { settings = settings { combatAnimations = not $ combatAnimations settings } , .. }
