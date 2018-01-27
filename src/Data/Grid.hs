{-# LANGUAGE NamedFieldPuns #-}
module Data.Grid where
  data Direction = N | NE | E | SE | S | SW | W | NW

  data Grid a = Grid
    { width  :: Int
    , height :: Int
    , cells  :: [a]
    }
  instance Functor Grid where
    fmap f g = g { cells = fmap f (cells g) }
  instance Foldable Grid where
    foldr f b Grid{ cells } = foldr f b cells

  squareGrid :: Int -> a -> Grid a
  squareGrid s = newGrid s s
  square = squareGrid

  newGrid :: Int -> Int -> a -> Grid a
  newGrid w h element = Grid w h $ replicate (w * h) element
  new = newGrid

  fromLists :: [[a]] -> Grid a
  fromLists lists = Grid width height cells
    where
      width = minimum $ fmap length lists
      height = length lists
      cells = take (width * height) $ concat lists

  positionIn :: Grid a -> Int -> (Int, Int)
  positionIn Grid { width } index = (index `mod` width, index `div` width)

  indexOf :: Int -> Int -> Grid a -> Int
  indexOf x y Grid { width } = y * width + x

  cellAt :: Int -> Int -> Grid a -> a
  cellAt x y grid = cellAtIndex (indexOf x y grid) grid

  cellAtIndex :: Int -> Grid a -> a
  cellAtIndex i g = cells g !! i

  insertAt :: Int -> Int -> Grid a -> a -> Grid a
  insertAt x y grid element = updateAt x y grid (const element)

  updateAt :: Int -> Int -> Grid a -> (a -> a) -> Grid a
  updateAt x y grid f = grid { cells = updateAt_ (indexOf x y grid) (cells grid) f }
    where
      updateAt_ :: Int -> [a] -> (a -> a) -> [a]
      updateAt_ 0 (a : as) f = f a : as
      updateAt_ n (a : as) f = a : updateAt_ (n - 1) as f
      updateAt_ _ [] _ = undefined

  isTop :: Int -> Grid a -> Bool
  isTop = const . (==) 0

  isLeft :: Int -> Grid a -> Bool
  isLeft = const . (==) 0

  isBottom :: Int -> Grid a -> Bool
  isBottom y Grid { height } = y == height - 1

  isRight :: Int -> Grid a -> Bool
  isRight x Grid { width } = x == width - 1

  isEdge :: Int -> Int -> Grid a -> Bool
  isEdge x y grid = isLeft x grid || isRight x grid || isTop y grid || isBottom y grid

  isCorner :: Int -> Int -> Grid a -> Bool
  isCorner x y grid = (isLeft x grid || isRight x grid) && (isTop y grid || isBottom y grid)

  neighbour :: Direction -> Int -> Int -> Grid a -> Maybe Int
  neighbour N x y grid  | isTop y grid    = Nothing
                        | otherwise       = Just $ indexOf x (y - 1) grid
  neighbour S x y grid  | isBottom y grid = Nothing
                        | otherwise       = Just $ indexOf x (y + 1) grid
  neighbour E x y grid  | isRight x grid  = Nothing
                        | otherwise       = Just $ indexOf (x + 1) y grid
  neighbour W x y grid  | isLeft x grid   = Nothing
                        | otherwise       = Just $ indexOf (x - 1) y grid
  neighbour NW x y grid | isTop y grid || isLeft x grid     = Nothing
                        | otherwise                         = Just $ indexOf (x - 1) (y - 1) grid
  neighbour NE x y grid | isTop y grid || isRight x grid    = Nothing
                        | otherwise                         = Just $ indexOf (x + 1) (y - 1) grid
  neighbour SW x y grid | isBottom y grid || isLeft x grid  = Nothing
                        | otherwise                         = Just $ indexOf (x - 1) (y + 1) grid
  neighbour SE x y grid | isBottom y grid || isRight x grid = Nothing
                        | otherwise                         = Just $ indexOf (x + 1) (y + 1) grid

  distance :: Int -> Int -> Int -> Int -> Int
  distance x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)
