module Action (eventLoop) where
  import Control.Concurrent
  import SDL
  import Lib.Model.Game
  import Lib.Action.MainMenu

  eventLoop :: MVar Game -> IO ()
  eventLoop model = do
    event <- waitEvent
    takeMVar model >>= processEvent event >>= putMVar model >> eventLoop model

  processEvent :: Event -> Game -> Either () (IO Game)
  processEvent (Event timestamp payload) game =
    case payload of
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEscape _ _)) -> Left ()
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeDown _ _)) -> Right $ nextOption game
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeUp _ _)) -> Right $ previousOption game
      KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym ScancodeEnter _ _)) -> Right $ selectOption game
      _ -> Right $ return game
