module Lib.Render (renderLoop) where
  import Control.Concurrent.MVar
  import Control.Monad.State
  import SDL (clear, present, Renderer)
  import qualified SDL.Image
  import qualified SDL.Font
  import Lib.Model.Game
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
    getRenderer >>= clear
    view game
    getRenderer >>= present
    renderLoop_ model
