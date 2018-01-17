module Main where
  import Control.Concurrent
  import Model

  main :: IO ()
  main = do
    model <- newMVar startGame
    forkIO $ do
      -- Rendering thread
      return ()
    forkIO $ do
      -- Audio thread
      return ()
    forkIO $ do
      -- Game loop thread
      return ()
    -- Interaction thread
