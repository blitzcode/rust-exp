
{-# LANGUAGE   RecordWildCards
             , ForeignFunctionInterface
             , TemplateHaskell
             , FlexibleContexts #-}

module RustRasterizerExperiment ( RustRasterizerExperiment
                                ) where

import Control.Monad.IO.Class
import Control.Lens hiding (Index)
import Control.Monad.State.Class
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.Word
import Data.Maybe
import Text.Printf
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Graphics.UI.GLFW as GLFW

import Experiment
import FrameBuffer
import Timing
import qualified BoundedSequence as BS
import Median
import GLFWHelpers
import WrapEnum

-- Software Rasterization

data Mode = Point | Line | Fill
            deriving (Eq, Show, Enum, Bounded)

data Index = Index { idxVal :: !Int
                   , idxMax :: !Int
                   }

idxSucc :: Index -> Index
idxSucc Index { .. } | idxVal >= idxMax = Index 0            idxMax
                     | otherwise        = Index (idxVal + 1) idxMax

idxPred :: Index -> Index
idxPred Index { .. } | idxVal <= 0 = Index idxMax       idxMax
                     | otherwise   = Index (idxVal - 1) idxMax

instance Show Index where
    show Index { .. } = printf "%i/%i" (idxVal + 1) (idxMax + 1)

data RustRasterizerExperiment = RustRasterizerExperiment
    { _rrTimes         :: !(BS.BoundedSequence Double)
    , _rrShadePerPixel :: !Bool
    , _rrMode          :: !Mode
    , _rrMeshIdx       :: !Index
    , _rrShaderIdx     :: !Index
    , _rrCMIdx         :: !Index
    , _rrBgIdx         :: !Index
    }

makeLenses ''RustRasterizerExperiment

instance Experiment RustRasterizerExperiment where
    withExperiment f = do
        num_mesh   <- fromIntegral <$> rastGetNumMeshes
        num_shader <- fromIntegral <$> rastGetNumShaders
        num_cm     <- fromIntegral <$> rastGetNumCMSets
        num_bg     <- fromIntegral <$> rastGetNumBackgrounds
        f $ RustRasterizerExperiment { _rrTimes         = BS.empty 60
                                     , _rrShadePerPixel = False
                                     , _rrMode          = Fill
                                     , _rrMeshIdx       = Index 5 (num_mesh   - 1)
                                     , _rrShaderIdx     = Index 3 (num_shader - 1)
                                     , _rrCMIdx         = Index 0 (num_cm     - 1)
                                     , _rrBgIdx         = Index 0 (num_bg     - 1)
                                     }
    experimentName _ = "RustRasterizer"
    experimentDraw fb tick = do
        RustRasterizerExperiment { .. } <- get
        mbtime <- liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                fst <$> timeIt (rastDraw (fromIntegral $ fromEnum _rrShadePerPixel)
                                         (fromIntegral $ fromEnum _rrMode         )
                                         (fromIntegral $ idxVal _rrMeshIdx        )
                                         (fromIntegral $ idxVal _rrShaderIdx      )
                                         (fromIntegral $ idxVal _rrCMIdx          )
                                         (fromIntegral $ idxVal _rrBgIdx          )
                                         (realToFrac tick)
                                         (fromIntegral w)
                                         (fromIntegral h)
                                         pvec)
        case mbtime of
            Just time -> rrTimes %= BS.push_ time
            Nothing   -> return ()
    experimentStatusString = do
        RustRasterizerExperiment { .. } <- get
        let avgtime    = fromMaybe 1 . median . BS.toList $ _rrTimes
            mesh_idx   = fromIntegral $ idxVal _rrMeshIdx
            shader_idx = fromIntegral $ idxVal _rrShaderIdx
            cm_idx     = fromIntegral $ idxVal _rrCMIdx
        num_tri     <- liftIO $ rastGetMeshTriCnt mesh_idx
        mesh_name   <- liftIO $ rastGetMeshName    mesh_idx  >>= peekCString
        shader_name <- liftIO $ rastGetShaderName shader_idx >>= peekCString
        cm_name     <- liftIO $ rastGetCMSetName  cm_idx     >>= peekCString
        return $ printf ( "%.1fFPS/%.2fms | [M]ode: %s\n" ++
                          "%s | Background [1][2] %s | Mesh [Q][W] %s: %s (%s)\n" ++
                          "Shader [A][S] %s: %s | Environment [Z][X] %s: %s"
                        )
                        (1 / avgtime)
                        (avgtime * 1000)
                        (show _rrMode)
                        ( if   _rrShadePerPixel
                          then "[P]erPixel"
                          else "[P]erVertex"
                        )
                        (show _rrBgIdx)
                        (show _rrMeshIdx)
                        mesh_name
                        ( if   num_tri > 1000
                          then printf "%.1fK Tri" (fromIntegral num_tri / 1000 :: Float)
                          else printf "%i Tri"    (fromIntegral num_tri        :: Int  )
                          :: String
                        )
                        (show _rrShaderIdx)
                        shader_name
                        (show _rrCMIdx)
                        cm_name
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'M -> rrMode          %= wrapSucc
                    GLFW.Key'P -> rrShadePerPixel %= not
                    GLFW.Key'Q -> rrMeshIdx       %= idxPred
                    GLFW.Key'W -> rrMeshIdx       %= idxSucc
                    GLFW.Key'A -> rrShaderIdx     %= idxPred
                    GLFW.Key'S -> rrShaderIdx     %= idxSucc
                    GLFW.Key'Z -> rrCMIdx         %= idxPred
                    GLFW.Key'X -> rrCMIdx         %= idxSucc
                    GLFW.Key'1 -> rrBgIdx         %= idxPred
                    GLFW.Key'2 -> rrBgIdx         %= idxSucc
                    _          -> return ()
            _ -> return ()

foreign import ccall "rast_draw" rastDraw :: CInt       -- Shader per-pixel (bool)?
                                          -> CInt       -- Render mode enum
                                          -> CInt       -- Mesh index
                                          -> CInt       -- Shader index
                                          -> CInt       -- Environment map index
                                          -> CInt       -- Background index
                                          -> CDouble    -- Tick
                                          -> CInt       -- Framebuffer width
                                          -> CInt       -- Framebuffer height
                                          -> Ptr Word32 -- Framebuffer pointer
                                          -> IO ()

foreign import ccall "rast_get_num_meshes" rastGetNumMeshes :: IO CInt -- Number of meshes

foreign import ccall "rast_get_mesh_name" rastGetMeshName :: CInt       -- Mesh index
                                                          -> IO CString -- Mesh name

foreign import ccall "rast_get_mesh_tri_cnt" rastGetMeshTriCnt :: CInt    -- Mesh index
                                                               -> IO CInt -- Triangle count

foreign import ccall "rast_get_num_shaders" rastGetNumShaders :: IO CInt -- Number of shaders

foreign import ccall "rast_get_shader_name" rastGetShaderName :: CInt       -- Shader index
                                                              -> IO CString -- Shader name

foreign import ccall "rast_get_num_cm_sets" rastGetNumCMSets :: IO CInt -- Number of CM sets

foreign import ccall "rast_get_cm_set_name" rastGetCMSetName :: CInt       -- CM index
                                                             -> IO CString -- CM set name

foreign import ccall "rast_get_num_backgrounds" rastGetNumBackgrounds :: IO CInt -- Number of BGs

