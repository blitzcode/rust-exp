
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module GLHelpers ( getGLStrings
                 , getGLExtensionList
                 , traceOnGLError
                 , throwOnGLError
                 , getCurTex2DSize
                 , disableVAOAndShaders
                 , Transparency(..)
                 , setTransparency
                 , setTextureFiltering
                 , setTextureClampST
                 , TextureFiltering(..)
                 , setupViewport
                 , maxRenderSize
                 , genObjectNameResource
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLR
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Exception
import Control.Monad.Trans.Resource
import Text.Printf
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String

import Trace

-- Various utility functions related to OpenGL

getErrors :: Maybe String -> IO (Maybe String)
getErrors context =
    GL.get GL.errors >>= \case
        []  -> return Nothing
        err -> return . Just $
                   "OpenGL Error" ++ maybe ": " (\c -> " (" ++ c ++ "): ") context ++ show err

traceOnGLError :: Maybe String -> IO ()
traceOnGLError context = getErrors context >>= maybe (return ()) (traceS TLError)

throwOnGLError :: Maybe String -> IO ()
throwOnGLError context = getErrors context >>= maybe (return ()) (throwIO . ErrorCall)

-- No wrapper around the OpenGL 3 extension APIs yet, have to use the raw ones
getNumExtensions :: IO Int
getNumExtensions =
    alloca $ \(ptr :: Ptr GLR.GLint) ->
        GLR.glGetIntegerv GLR.GL_NUM_EXTENSIONS ptr >> fromIntegral <$> peek ptr
getExtensionStr :: Int -> IO String
getExtensionStr i =
    peekCString =<< castPtr <$> GLR.glGetStringi GLR.GL_EXTENSIONS (fromIntegral i)

getGLExtensionList :: IO [String]
getGLExtensionList =
  getNumExtensions >>= \numExt -> forM [0..numExt - 1] $ \i -> getExtensionStr i

-- Take the minimum of the maximum viewport and texture size to figure out
-- how large of a frame buffer we can allocate and render into
maxRenderSize :: IO (Int, Int)
maxRenderSize =
    withArray [0, 0] $ \ptr -> do
        GLR.glGetIntegerv GLR.GL_MAX_VIEWPORT_DIMS ptr
        [vpWdh, vpHgt] <- peekArray 2 ptr
        GLR.glGetIntegerv GLR.GL_MAX_TEXTURE_SIZE ptr
        texDim <- peek ptr
        return (fromIntegral $ min vpWdh texDim, fromIntegral $ max vpHgt texDim)

getGLStrings :: IO String
getGLStrings = do
  numExt <- getNumExtensions
  (w, h) <- maxRenderSize
  printf
      ( "OpenGL - Vendor: %s · Renderer: %s · Version: %s · GLSL: %s · Num Extensions: %i" ++
        " · Max FB Res: %ix%i\nGLFW   - Version: %s"
      )
      <$> GL.get GL.vendor
      <*> GL.get GL.renderer
      <*> GL.get GL.glVersion
      <*> GL.get GL.shadingLanguageVersion
      <*> pure numExt
      <*> pure w
      <*> pure h
      <*> (fromJust <$> GLFW.getVersionString)

getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\(GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D GL.Texture2D 0)

data TextureFiltering = TFNone | TFMinMag | TFMinOnly | TFMagOnly

setTextureFiltering :: GL.ParameterizedTextureTarget t => t -> TextureFiltering -> IO ()
setTextureFiltering target TFNone =
    GL.textureFilter target GL.$= ((GL.Nearest, Nothing        ), GL.Nearest)
setTextureFiltering target TFMinMag =
    GL.textureFilter target GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
setTextureFiltering target TFMinOnly =
    GL.textureFilter target GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
setTextureFiltering target TFMagOnly =
    GL.textureFilter target GL.$= ((GL.Nearest, Nothing        ), GL.Linear')

setTextureClampST :: GL.ParameterizedTextureTarget t => t -> IO ()
setTextureClampST target =
    forM_ [GL.S, GL.T] $
        \x -> GL.textureWrapMode target x GL.$= (GL.Repeated, GL.ClampToEdge)

data Transparency = TRNone
                  | TRBlend !Float
                  | TRSrcAlpha
                  deriving (Eq, Ord, Show)

setTransparency :: Transparency -> IO ()
setTransparency trans =
    case trans of TRNone -> GL.blend GL.$= GL.Disabled
                  TRBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
                  TRSrcAlpha -> do
                      GL.blend     GL.$= GL.Enabled
                      GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

-- Disable vertex attribute arrays and shaders
disableVAOAndShaders :: IO ()
disableVAOAndShaders = do
    GL.bindVertexArrayObject GL.$= Nothing
    GL.currentProgram        GL.$= Nothing

setupViewport :: Int -> Int -> IO ()
setupViewport w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

-- Allocate OpenGL object name in ResourceT
genObjectNameResource :: (GL.GeneratableObjectName a, MonadResource m) => m a
genObjectNameResource = snd <$> allocate GL.genObjectName GL.deleteObjectName

