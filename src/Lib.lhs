\documentclass{article}
\usepackage{../literate}

\begin{document}

\module{Lib}

This module exposes the \ident{play} function which, when called, starts up and runs the entire
game.

It is in this function that SDL is initialized, and the threads for graphics, video, processing, and
user input are created. By the time this function returns, the game window is closed.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Lib (play) where
  import Control.Concurrent
  import Control.Monad
  import Foreign.C.Types
  import SDL
  import qualified Lib.Model.Game as Game
  import Lib.Render
  import Lib.Action

  play :: IO ()
  play = do
\end{code}

The first step is to initialize SDL and open up a window with a renderer. This is all pretty
standard SDL behaviour.

\begin{code}
    initializeAll -- TODO: probably don't need all systems
    -- TODO: figure out how graphics settings will work
    window <- createWindow "Definitely Not Fire Emblem"
      ( defaultWindow
          { windowBorder = False
          , windowMode = FullscreenDesktop
          , windowInitialSize = V2 (CInt 1920) (CInt 1080)
          }
      )
    renderer <- createRenderer window (-1)
      ( defaultRenderer
          { rendererType = AcceleratedVSyncRenderer
          }
      )
\end{code}

Next is to initialize the game-related constructs. Basically just creating an instance of the
\ident{Model}. In future versions, this is where a save file may be read in to restore the user's
settings to how they had them when the game was last closed.

As the model is going to be shared across threads, it is placed into a simple \ident{MVar}. After
some thought, it was determined that the requirements this game has in terms of processing power and
need for fully optimizable model updates is relatively low, so simplicity of the implementation is
the way to go. % TODO: if this gets bad, reconsider!

\begin{code}
    model <- newMVar Game.new
    audioChannel <- newChan
\end{code}

Finally come the four main threads of the game, in no particular order.

The first is the rendering thread. At each step it clears the screen, reads in the model, renders
them, and presents the final image to the player. The model it is using, however, is a copy of the
one that is in the \ident{MVar}. This way the renderer can render an entire frame while the other
threads continue to process inputs and update the model. Since this game does not require frame
perfect interactions, we can sacrifice consistency for speed, to some extent.

\begin{code}
    renderThread <- forkIO $ renderLoop model renderer
\end{code}

The next thread is the audio thread. The audio thread awaits signals on the channel shared with the
other threads, and plays the requested sounds at the next convenient opportunity.

\begin{code}
    audioThread <- forkIO $ do
      let
        playSound = do
          audio <- readChan audioChannel
          -- TODO: play the sound
          playSound
      return ()
\end{code}

Then comes the interaction loop thread. This thread is the one that processes all of the user inputs
and performs the updates to the model.

\begin{code}
    eventLoop model
\end{code}

Once the event loop decides it is time to end the other threads are killed and then we shut down
SDL. Killing the threads ensures that they end gracefully, instead of in segmentation faults or
other strange errors.

\begin{code}
    killThread renderThread
    killThread audioThread
    quit
\end{code}

\end{document}
