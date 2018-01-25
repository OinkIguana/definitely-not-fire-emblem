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
      MouseMotionEvent mouseMotionData -> mouseMotionEvent mouseMotionData
      MouseButtonEvent mouseButtonData -> mouseButtonEvent mouseButtonData
      MouseWheelEvent mouseWheelData -> mouseWheelEvent mouseWheelData
      _ -> return

  -- Keyboard events
  keyboardEvent :: KeyboardEventData -> Game -> IO Game
  keyboardEvent (KeyboardEventData _ Pressed _ (Keysym scancode _ _)) = keyPressed scancode
  keyboardEvent (KeyboardEventData _ Released _ (Keysym scancode _ _)) = keyReleased scancode
  keyboardEvent _ = return

  keyPressed :: Scancode -> Game -> IO Game
  keyPressed ScancodeDown = nextOption
  keyPressed ScancodeUp = previousOption
  keyPressed ScancodeReturn = selectOption
  keyPressed _ = return

  keyReleased :: Scancode -> Game -> IO Game
  keyReleased _ = return

  -- Mouse events
  mouseMotionEvent :: MouseMotionEventData -> Game -> IO Game
  mouseMotionEvent (MouseMotionEventData _ _ buttons position delta) = mouseMoved buttons position delta

  mouseMoved :: [MouseButton] -> Point V2 Int32 -> V2 Int32 -> Game -> IO Game
  mouseMoved _ _ _ = return

  mouseButtonEvent :: MouseButtonEventData -> Game -> IO Game
  mouseButtonEvent (MouseButtonEventData _ Pressed _ button _ position) = mouseButtonPressed button position
  mouseButtonEvent (MouseButtonEventData _ Released _ button _ position) = mouseButtonReleased button position

  mouseButtonPressed :: MouseButton -> Point V2 Int32 -> Game -> IO Game
  mouseButtonPressed _ _ = return

  mouseButtonReleased :: MouseButton -> Point V2 Int32 -> Game -> IO Game
  mouseButtonReleased _ _ = return

  mouseWheelEvent :: MouseWheelEventData -> Game -> IO Game
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ 0) _) = return
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ y) ScrollFlipped)  | y < 0 = mouseScrolledUp (-y)
                                                                    | y > 0 = mouseScrolledDown y
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ y) ScrollNormal)   | y < 0 = mouseScrolledDown (-y)
                                                                    | y > 0 = mouseScrolledUp y

  mouseScrolledDown :: Int32 -> Game -> IO Game
  mouseScrolledDown _ = return

  mouseScrolledUp :: Int32 -> Game -> IO Game
  mouseScrolledUp _ = return
