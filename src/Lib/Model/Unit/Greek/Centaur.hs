{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Lib.Model.Unit.Greek.Centaur
  ( basic
  , chiron
  ) where
  import Lib.Model

  baseStats :: Stats
  baseStats = Stats
    { mhp = 32
    , chp = 32
    , atk = 19
    , mag = 7
    , def = 19
    , res = 8
    , spd = 24
    , lck = 14
    , skl = 19
    , mov = 8
    }

  basic :: Unit
  basic = Unit Centaur "Centaur warrior" baseStats [OneOf [Spear, Bow]] [Trample] (Sprite Invisible)

  chiron :: Unit
  chiron = basic
    { name = "Chiron"
    , stats = baseStats { mhp = 48, chp = 48, atk = 21, def = 20, lck = 9, skl = 29 } }
