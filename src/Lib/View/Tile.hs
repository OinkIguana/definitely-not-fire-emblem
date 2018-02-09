module Lib.View.Tile (view) where
  import Prelude.Unicode
  import Control.Monad.Unicode
  import Data.Shape
  import qualified Data.Set as Set
  import SDL hiding (Point, unit)
  import Lib.Model
  import Lib.RC
  import Lib.Constants
  import qualified Lib.View.Unit as Unit

  view ∷ Game → Point Int → Tile → StateRC ()
  view game position Tile { terrain, unit, highlight } = do
    texture ← getTexture $ textureFor terrain
    renderer ← getRenderer
    let cellPosition = Just $ toSDL $ vectorRectangle scaledPosition tileSize
    copy renderer texture Nothing cellPosition
    let textures = fmap (getTexture ∘ highlightFor) $ Set.toList highlight
    mapM_ ((=≪) (\t → copy renderer t Nothing cellPosition)) textures
    case unit of
      Nothing   → return ()
      Just unit → Unit.viewAtTile scaledPosition unit
    where Point x y = position
          Dimension w h = tileSize
          scaledPosition = Point (fromIntegral x * w) (fromIntegral y * h)

  textureFor ∷ Terrain → Key SDL.Texture
  textureFor Plain = keyFor "Lib.View.Tile::Terrain.Plain" plain
  textureFor _     = undefined

  plain ∷ StateRC Texture
  plain = do
    renderer ← getRenderer
    texture ← createTexture renderer RGBA8888 TextureAccessTarget $ toSDL tileSize
    rendererRenderTarget renderer $= Just texture
    rendererDrawColor renderer $= V4 0 255 0 255
    fillRect renderer Nothing
    rendererDrawColor renderer $= V4 0 0 0 255
    drawRect renderer Nothing
    rendererRenderTarget renderer $= Nothing
    return texture


  highlightFor ∷ TileHighlight → Key SDL.Texture
  highlightFor Select = keyFor "Lib.View.Tile::TileHighlight.Selected" selected
  highlightfor _      = undefined

  selected ∷ StateRC Texture
  selected = do
    renderer ← getRenderer
    texture ← createTexture renderer RGBA8888 TextureAccessTarget $ toSDL tileSize
    rendererRenderTarget renderer $= Just texture
    rendererDrawColor renderer $= V4 255 255 255 120
    fillRect renderer Nothing
    rendererDrawColor renderer $= V4 0 0 0 255
    drawRect renderer Nothing
    rendererRenderTarget renderer $= Nothing
    return texture
