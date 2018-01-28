module Lib.Action (eventLoop) where
  import Data.Int
  import Control.Concurrent
  import Control.Monad
  import SDL (V2(..), Point)
  import SDL.Event
  import SDL.Input.Keyboard
  import SDL.Input.Mouse
  import Lib.Model hiding (Point)

  import qualified Lib.Action.MainMenu as MainMenu
  import qualified Lib.Action.PauseMenu as PauseMenu
  import Lib.Action.Game (pause)

  eventLoop :: MVar Game -> IO ()
  eventLoop model = do
    event <- waitEvent
    game <- takeMVar model >>= processEvent event
    putMVar model game
    unless (quit game) $ eventLoop model

  processEvent :: Event -> Action
  processEvent (Event timestamp payload) =
    case payload of
      KeyboardEvent keyboardData -> keyboardEvent keyboardData
      MouseMotionEvent mouseMotionData -> mouseMotionEvent mouseMotionData
      MouseButtonEvent mouseButtonData -> mouseButtonEvent mouseButtonData
      MouseWheelEvent mouseWheelData -> mouseWheelEvent mouseWheelData
      _ -> return

  -- Keyboard events
  keyboardEvent :: KeyboardEventData -> Action
  keyboardEvent (KeyboardEventData _ Pressed _ (Keysym scancode _ _)) = keyPressed scancode
  keyboardEvent (KeyboardEventData _ Released _ (Keysym scancode _ _)) = keyReleased scancode
  keyboardEvent _ = return

  keyPressed :: Scancode -> Action
  keyPressed ScancodeDown = MainMenu.nextOption >=> PauseMenu.nextOption
  keyPressed ScancodeUp = MainMenu.previousOption >=> PauseMenu.previousOption
  keyPressed ScancodeReturn = MainMenu.selectOption >=> PauseMenu.selectOption
  keyPressed ScancodeEscape = pause
  keyPressed _ = return

  keyReleased :: Scancode -> Action
  keyReleased _ = return

  -- Mouse events
  mouseMotionEvent :: MouseMotionEventData -> Action
  mouseMotionEvent (MouseMotionEventData _ _ buttons position delta) = mouseMoved buttons position delta

  mouseMoved :: [MouseButton] -> Point V2 Int32 -> V2 Int32 -> Action
  mouseMoved _ _ _ = return

  mouseButtonEvent :: MouseButtonEventData -> Action
  mouseButtonEvent (MouseButtonEventData _ Pressed _ button _ position) = mouseButtonPressed button position
  mouseButtonEvent (MouseButtonEventData _ Released _ button _ position) = mouseButtonReleased button position

  mouseButtonPressed :: MouseButton -> Point V2 Int32 -> Action
  mouseButtonPressed _ _ = return

  mouseButtonReleased :: MouseButton -> Point V2 Int32 -> Action
  mouseButtonReleased _ _ = return

  mouseWheelEvent :: MouseWheelEventData -> Action
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ 0) _) = return
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ y) ScrollFlipped)  | y < 0 = mouseScrolledUp (-y)
                                                                    | y > 0 = mouseScrolledDown y
  mouseWheelEvent (MouseWheelEventData _ _ (V2 _ y) ScrollNormal)   | y < 0 = mouseScrolledDown (-y)
                                                                    | y > 0 = mouseScrolledUp y

  mouseScrolledDown :: Int32 -> Action
  mouseScrolledDown _ = return

  mouseScrolledUp :: Int32 -> Action
  mouseScrolledUp _ = return
