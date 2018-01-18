\documentclass{article}
\usepackage{../../literate}

\begin{document}

\module[Lib]{Model}

As the model for an entire game is understandably complex, this module provides a few helpful
methods for dealing with the model. The actual model itself is described in the
\ident{Lib.Module.Game} module.

\begin{code}
module Lib.Model where
  import Lib.Model.Game
\end{code}

The \ident{newGame} is the initial state of the game, as it is when started up fresh. From here,
through use of \ident{Action}s the game unfolds.

\begin{code}
  newGame :: Game
  newGame = Game defaultSettings mainMenu []

  defaultSettings :: Settings
  defaultSettings = Settings True True True

  mainMenu :: Scene
  mainMenu = MainMenu $ menu []

  menu :: [(String, Action)] -> Menu
  menu options = Menu options 0 Nothing
\end{code}

\end{document}
