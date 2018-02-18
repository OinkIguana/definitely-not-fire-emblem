module Lib.Action.Board (selectCellAtPoint) where
  import Prelude.Unicode
  import Data.Shape
  import Data.Grid
  import Data.Int
  import Data.Maybe
  import qualified Data.Set as Set
  import Control.Monad
  import qualified SDL (Point, V2)
  import Lib.Model
  import Lib.Model.Board hiding (indexOf)
  import Lib.Constants

  selectCellAtPoint ∷ SDL.Point SDL.V2 Int32 → Action
  selectCellAtPoint sdlPoint game =
    case (findPreviouslySelected board, clickedIndex) of
      (Nothing, Just index) →
        let newBoard = selectedAt index board
         in return Game { room = Battlefield Battle { board = newBoard, .. }, .. }
      (Just selection, Just index) →
        let newBoard = deselect $ swapContents selection index board
         in return Game { room = Battlefield Battle { board = newBoard, .. }, .. }
      otherwise → return game
    where
      Game { room = Battlefield Battle { board, .. }, .. } = game
      clickedIndex = indexOf (floor x) (floor y) (grid board)
      Dimension w h = tileSize
      Rectangle x y _ _ = project (Dimension (fromIntegral w) (fromIntegral h)) (Rectangle 0 0 1 1) (fromSDL sdlPoint :: Point Float)

  selectedAt ∷ Int → Board → Board
  selectedAt index board =
    board { grid = (grid board) { cells = fmap (uncurry $ selectAt index) indexedGrid } }
      where
        indexedGrid = zip [0..] $ cells (grid board)
        selectAt ∷ Int → Int → Tile → Tile
        selectAt target point tile =
          if target == point
            then tile { highlight = Set.insert Select $ highlight tile }
            else tile { highlight = Set.delete Select $ highlight tile }

  deselect ∷ Board → Board
  deselect board = board { grid = fmap (\tile → tile { highlight = Set.delete Select $ highlight tile }) $ grid board }

  findPreviouslySelected ∷ Board → Maybe Int
  findPreviouslySelected Board { grid } = findIndexOf (Set.member Select ∘ highlight) grid

  swapContents ∷ Int → Int → Board → Board
  swapContents a b board =
    board { grid = insertAt xb yb (tileb { unit = unita })
                    $ insertAt xa ya (tilea { unit = unitb })
                    $ grid board }
    where
      (xa, ya) = positionIn (grid board) a
      (xb, yb) = positionIn (grid board) b
      tilea = fromJust $ cellAt xa ya (grid board)
      tileb = fromJust $ cellAt xb yb (grid board)
      unita = unit tilea
      unitb = unit tileb
