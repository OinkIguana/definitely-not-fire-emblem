\begin{document}

To begin, note that we are using \texttt{Text} in place of \texttt{String} for the obvious reasons.
Additionally, to support proper blending of colours into the game sprites -- to differentiate the
teams -- we require the use of the \texttt{Colour} package.

The \texttt{DuplicateRecordFields} language extension is enabled as many of the components of the
model have conflicting names, but I feel they are most easily manageable when laid out in a single
file as it is done here.

\begin{code}
{-# LANGUAGE DuplicateRecordFields #-}

module Model where
  import Data.Text (Text)
  import Data.Colour (Colour)
\end{code}

The \texttt{Game} is what holds everything together, and serves as the model that is used to
represent the current state of the game at any given time. As in the usual fashion, 

\begin{code}
  data Game = Game
    { settings :: Settings
    , room     :: Room
    , graphics :: [Sprite]
    }

  data Settings = Settings
    -- TODO
    { combatAnimations   :: Bool
    , movementAnimations :: Bool
    , autoEnd            :: Bool
    }

  data Room
    = MainMenu Menu
    | PauseMenu Menu
    | Battle
      { players      :: [Player]
      , board        :: Board
      , turnCount    :: Int
      }

  data Menu = Menu
    { options   :: [String, () -> IO ()]
    , selection :: Int
    , submenu   :: Maybe Menu
    }

  data Player
    = Human
      { name   :: Text
      , colour :: Colour
      , units  :: [Unit]
      }
    | CPU
      { colour :: Colour
      , units  :: [Unit]
      }
    | Neutral
      { units  :: [Unit] }

  data Controller = Human | CPU

  data Unit = Unit
    { class     :: Class
    , name      :: Text
    , stats     :: Stats
    , equipment :: Maybe Equipment
    , skills    :: [Skill]
    , owner     :: GameRef Player
    }

  data Stats = Stats
    { mhp :: Int
    , chp :: Int
    , atk :: Int
    , mag :: Int
    , def :: Int
    , res :: Int
    , spd :: Int
    , mov :: Int
    , lck :: Int
    , skl :: Int
    }

  data Skill = Skill -- TODO

  data Class
    = Tank
    | Infantry
    | Archer
    | Cavalry
    | Flyer
    | CavalryArcher
    | CavalryTank
    | FlyerArcher
    -- TODO

  data Board = Board
    { grid :: Grid Tile
    }

  data Grid a = Grid
    { width  :: Int
    , height :: Int
    , cells  :: [a]
    }

  data Tile = Tile
    { terrain :: Terrain
    , unit    :: Maybe (GameRef Unit)
    }

  data Terrain
    = Plain
    | Mountain
    | Forest
    | Swamp
    | River
    | Water
    | Hill
    | Road
    -- TODO

  data MapData = MapData
    { board          :: Board
    , startPositions :: [GameRef Tile]
    }

  newtype GameRef a = GameRef (Game -> a)
\end{code}

\end{document}
