{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Lib.Model.Unit.Stealth.Assassin where
  import Lib.Model

  baseStats :: Stats
  baseStats = Stats
    { mhp = 21
    , chp = 21
    , atk = 15
    , def = 2
    , spd = 13
    , lck = 6
    , skl = 27
    , mov = 5
    , snk = 54
    , vis = 4
    }

  basic :: Unit
  basic = Unit Assassin "Assassin" baseStats [Knife] [SneakAttack] (Sprite Invisible)
