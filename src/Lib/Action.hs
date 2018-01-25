module Lib.Action (eventLoop) where
  import Control.Concurrent
  import Control.Monad
  import SDL.Event
  import SDL.Input.Keyboard
  import Lib.Model.Game
  import Lib.Action.MainMenu

  eventLoop :: MVar Game -> IO ()
  eventLoop model = do
    event <- waitEvent
    game <- takeMVar model >>= processEvent event
    putMVar model game
    unless (quit game) $ eventLoop model

  processEvent :: Event -> Game -> IO Game
  processEvent (Event timestamp payload) =
    case payload of
      KeyboardEvent keyboardData -> keyboardEvent keyboardData
      _ -> return

  keyboardEvent :: KeyboardEventData -> Game -> IO Game
  keyboardEvent (KeyboardEventData _ Pressed _ (Keysym scancode _ _)) = keyPressed scancode
  keyboardEvent _ = return

  keyPressed :: Scancode -> Game -> IO Game
  keyPressed ScancodeDown = nextOption
  keyPressed ScancodeUp = previousOption
  keyPressed ScancodeReturn = selectOption
  keyPressed _ = return
