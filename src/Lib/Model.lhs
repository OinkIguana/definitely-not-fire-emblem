\documentclass{article}
\usepackage{../../literate}

\begin{document}

\module[Lib]{Model}

As the model for an entire game is understandably complex, this module provides a few helpful
methods for dealing with the model. The actual model itself is described in the
\ident{Lib.Module.Game} module.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Model where
  import Data.Text
  import Lib.Model.Game
  import Lib.Action.Game as Game
\end{code}

The \ident{newGame} is the initial state of the game, as it is when started up fresh. Through
actions, this state is modified and the game is played.

\begin{code}
  newGame :: Game
  newGame = Game defaultSettings [] mainMenu False

  defaultSettings :: Settings
  defaultSettings = Settings True True True

  mainMenu :: Room
  mainMenu = MainMenu $
    menu
      [ ("New Game", noop)
      , ("Continue", noop)
      , ("Multiplayer", noop)
      , ("Settings", noop)
      , ("Quit", Game.quit) ]

  menu :: [(Text, Action)] -> Menu
  menu options = Menu options 0 Nothing

  noop :: Action
  noop = return
\end{code}

\end{document}
