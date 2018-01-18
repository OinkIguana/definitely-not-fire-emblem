\begin{code}
module Lib (play) where
  import Control.Concurrent
  import Lib.Model

  play :: IO ()
  play = do
    model <- newMVar newGame
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
    return ()
\end{code}
