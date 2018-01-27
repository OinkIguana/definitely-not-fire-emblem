module Lib.Render (renderLoop) where
  import Control.Concurrent.MVar
  import Control.Monad.State
  import SDL
  import qualified SDL.Image
  import qualified SDL.Font
  import Lib.Model
  import Lib.View
  import Lib.RC

  renderLoop :: MVar Game -> Renderer -> IO ()
  renderLoop model renderer = do
    SDL.Image.initialize [SDL.Image.InitPNG]
    SDL.Font.initialize
    runStateT (renderLoop_ model) $ newRC renderer
    SDL.Image.quit
    SDL.Font.quit

  renderLoop_ :: MVar Game -> StateRC ()
  renderLoop_ model = do
    game <- liftIO $ readMVar model
    renderer <- getRenderer
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    view game
    present renderer
    renderLoop_ model
