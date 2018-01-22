\documentclass{article}
\usepackage{../../literate}

\begin{document}

\module[Lib]{RC}

Short for rendering context, the \ident{RC} is intended to be used as \ident{StateRC}, allowing
textures, surfaces, and fonts to be created or loaded and stored for later use.

\begin{code}
  module Lib.RC
    ( module Control.Monad.State
    , Key
    , keyFor
    , RC
    , newRC
    , StateRC
    , getTexture
    , freeTexture
    , getSurface
    , freeSurface
    , getFont
    , freeFont
    , getRenderer
    ) where
    import Control.Monad.State
    import qualified Data.Map as Map
    import Data.Map (Map, (!?))
    import Data.Symbol
    import SDL (Surface, Texture, Renderer)
    import qualified SDL
    import SDL.Font (Font)
    import qualified SDL.Font as Font

    newtype Key a = Key (Key_ a)
    data Key_ a = Key_ Symbol (IO a)

    keyFor :: String -> IO a -> Key a
    keyFor str accessor = Key $ Key_ (intern str) accessor

    type StateRC a = StateT RC IO a

    newtype RC = RC RC_
    data RC_ = RC_
      { textures :: Map Symbol Texture
      , surfaces :: Map Symbol Surface
      , fonts    :: Map Symbol Font
      , renderer :: Renderer
      }

    newRC :: Renderer -> RC
    newRC = RC . RC_ Map.empty Map.empty Map.empty

    getRC :: StateRC RC_
    getRC = do
      RC rc <- get
      return rc

    putRC :: RC_ -> StateRC ()
    putRC = put . RC

    getsRC :: (RC_ -> a) -> StateRC a
    getsRC f = f <$> getRC

    addTexture :: Symbol -> Texture -> StateRC Texture
    addTexture symbol tex = do
      rc <- getRC
      putRC rc { textures = Map.insert symbol tex (textures rc) }
      return tex

    removeTexture :: Symbol -> StateRC ()
    removeTexture symbol = do
      rc <- getRC
      putRC rc { textures = Map.delete symbol (textures rc) }

    getTexture :: Key Texture -> StateRC Texture
    getTexture (Key (Key_ symbol accessor)) = do
      tex <- getsRC (flip (!?) symbol . textures)
      case tex of
        Nothing   -> liftIO accessor >>= addTexture symbol
        Just tex  -> return tex

    freeTexture :: Key Texture -> StateRC ()
    freeTexture (Key (Key_ symbol _)) = do
      tex <- getsRC (flip (!?) symbol . textures)
      case tex of
        Nothing   -> return ()
        Just tex -> do
          SDL.destroyTexture tex
          removeTexture symbol

    addSurface :: Symbol -> Surface -> StateRC Surface
    addSurface symbol surf = do
      rc <- getRC
      putRC rc { surfaces = Map.insert symbol surf (surfaces rc) }
      return surf

    removeSurface :: Symbol -> StateRC ()
    removeSurface symbol = do
      rc <- getRC
      putRC rc { surfaces = Map.delete symbol (surfaces rc) }

    getSurface :: Key Surface -> StateRC Surface
    getSurface (Key (Key_ symbol accessor)) = do
      surf <- getsRC (flip (!?) symbol . surfaces)
      case surf of
        Nothing   -> liftIO accessor >>= addSurface symbol
        Just surf -> return surf

    freeSurface :: Key Surface -> StateRC ()
    freeSurface (Key (Key_ symbol _)) = do
      surf <- getsRC (flip (!?) symbol . surfaces)
      case surf of
        Nothing   -> return ()
        Just surf -> do
          SDL.freeSurface surf
          removeSurface symbol

    addFont :: Symbol -> Font -> StateRC Font
    addFont symbol font = do
      rc <- getRC
      putRC rc { fonts = Map.insert symbol font (fonts rc) }
      return font

    removeFont :: Symbol -> StateRC ()
    removeFont symbol = do
      rc <- getRC
      putRC rc { fonts = Map.delete symbol (fonts rc) }

    getFont :: Key Font -> StateRC Font
    getFont (Key (Key_ symbol accessor)) = do
      font <- getsRC (flip (!?) symbol . fonts)
      case font of
        Nothing   -> liftIO accessor >>= addFont symbol
        Just font -> return font

    freeFont :: Key Font -> StateRC ()
    freeFont (Key (Key_ symbol _)) = do
      font <- getsRC (flip (!?) symbol . fonts)
      case font of
        Nothing   -> return ()
        Just font -> do
          Font.free font
          removeFont symbol

    getRenderer :: StateRC Renderer
    getRenderer = getsRC renderer
\end{code}

\end{document}
