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
  processEvent (Event timestamp payload) game =
    case payload of
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeDown _ _)) -> nextOption game
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeUp _ _)) -> previousOption game
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeReturn _ _)) -> selectOption game
      _ -> return game
