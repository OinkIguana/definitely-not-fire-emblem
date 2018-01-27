{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Lib.Model.Unit.Greek.Centaur
  ( basic
  , chiron
  ) where
  import Lib.Model

  baseStats :: Stats
  baseStats = Stats 32 32 19 7 19 8 24 14 19 8

  basic :: Unit
  basic = Unit Centaur "Centaur warrior" baseStats [OneOf [Spear, Bow]] [] (Sprite Invisible)

  chiron :: Unit
  chiron = basic
    { name = "Chiron"
    , stats = baseStats { mhp = 48, chp = 48, atk = 21, def = 20, lck = 9, skl = 29 } }
