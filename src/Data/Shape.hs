{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Shape where
  import qualified SDL
  data Num a ⇒ Point a     = Point     a a
    deriving (Show, Eq)
  data Num a ⇒ Dimension a = Dimension a a
    deriving (Show, Eq)
  data Num a ⇒ Rectangle a = Rectangle a a a a
    deriving (Show, Eq)

  vectorRectangle ∷ Num a ⇒ Point a → Dimension a → Rectangle a
  vectorRectangle (Point x y) (Dimension w h) = Rectangle x y w h

  scale ∷ (Num a, Shape s) ⇒ Rectangle a → s a → Rectangle a
  scale (Rectangle x y w h) shape = Rectangle x y (w * sx) (h * sy)
    where Dimension sx sy = toDimension shape

  shift ∷ (Num a, Shape s) ⇒ Rectangle a → s a → Rectangle a
  shift (Rectangle x y w h) shape = Rectangle (x + dx) (y + dy) w h
    where Point dx dy = toPoint shape

  project ∷ (Num a, Fractional a, Shape b, Shape c, Shape d) ⇒ b a → c a → d a → Rectangle a
  project from onto orig = Rectangle x y w h
    where Rectangle ox oy ow oh = toRectangle orig
          Rectangle sx sy sw sh = toRectangle from
          Rectangle dx dy dw dh = toRectangle onto
          xscale = dw / sw
          yscale = dh / sh
          xoffset = dx - sx
          yoffset = dy - sy
          x = (ox + xoffset) * xscale
          y = (oy + yoffset) * yscale
          w = ow * xscale
          h = oh * yscale

  class Shape s where
    toRectangle ∷ Num a ⇒ s a → Rectangle a
    toPoint     ∷ Num a ⇒ s a → Point a
    toDimension ∷ Num a ⇒ s a → Dimension a

  instance Shape Point where
    toRectangle (Point x y) = Rectangle x y 1 1
    toPoint = id
    toDimension _ = Dimension 1 1

  instance Shape Dimension where
    toRectangle (Dimension w h) = Rectangle 0 0 w h
    toPoint _ = Point 0 0
    toDimension = id

  instance Shape Rectangle where
    toRectangle = id
    toPoint (Rectangle x y _ _) = Point x y
    toDimension (Rectangle _ _ w h) = Dimension w h

  instance Num a ⇒ Num (Point a) where
    Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
    Point x1 y1 - Point x2 y2 = Point (x1 - x2) (y1 - y2)
    Point x1 y1 * Point x2 y2 = Point (x1 * x2) (y1 * y2)
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger i = Point (fromInteger i) (fromInteger i)

  instance Num a ⇒ Num (Dimension a) where
    Dimension x1 y1 + Dimension x2 y2 = Dimension (x1 + x2) (y1 + y2)
    Dimension x1 y1 - Dimension x2 y2 = Dimension (x1 - x2) (y1 - y2)
    Dimension x1 y1 * Dimension x2 y2 = Dimension (x1 * x2) (y1 * y2)
    abs (Dimension x y) = Dimension (abs x) (abs y)
    signum (Dimension x y) = Dimension (signum x) (signum y)
    fromInteger i = Dimension (fromInteger i) (fromInteger i)

  instance (Ord a, Num a) ⇒ Num (Rectangle a) where
    Rectangle x1 y1 w1 h1 + Rectangle x2 y2 _ _ = Rectangle (x1 + x2) (y1 + y2) w1 h1
    Rectangle x1 y1 w1 h1 - Rectangle x2 y2 _ _ = Rectangle (x1 - x2) (y1 - y2) w1 h1
    Rectangle x1 y1 w1 h1 * Rectangle _ _ w2 h2 = Rectangle x1 y1 (w1 * w2) (h1 * h2)
    abs (Rectangle x y w h) = Rectangle xx yy (abs w) (abs h)
      where xx = if w < 0 then x + w else x
            yy = if h < 0 then y + h else y
    signum (Rectangle x y w h) = Rectangle 0 0 (signum w) (signum h)
    fromInteger i = Rectangle 0 0 (fromInteger i) (fromInteger i)

  class ToSDL a b where
    toSDL   ∷ a → b
  class FromSDL a b where
    fromSDL ∷ a → b
  instance (Integral a, Num b) ⇒ ToSDL (Point a) (SDL.Point SDL.V2 b) where
    toSDL (Point x y) = SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)
  instance (Integral a, Num b) ⇒ FromSDL (SDL.Point SDL.V2 a) (Point b) where
    fromSDL (SDL.P (SDL.V2 x y)) = Point (fromIntegral x) (fromIntegral y)
  instance (Integral a, Num b) ⇒ ToSDL (Dimension a) (SDL.V2 b) where
    toSDL (Dimension x y) = SDL.V2 (fromIntegral x) (fromIntegral y)
  instance (Integral a, Num b) ⇒ FromSDL (SDL.V2 a) (Dimension b) where
    fromSDL (SDL.V2 x y) = Dimension (fromIntegral x) (fromIntegral y)
  instance (Integral a, Num b) ⇒ ToSDL (Rectangle a) (SDL.Rectangle b) where
    toSDL (Rectangle x y w h) = SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) (SDL.V2 (fromIntegral w) (fromIntegral h))
  instance (Integral a, Num b) ⇒ FromSDL (SDL.Rectangle a) (Rectangle b) where
    fromSDL (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) = Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
