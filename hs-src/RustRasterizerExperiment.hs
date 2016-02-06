
{-# LANGUAGE   RecordWildCards
             , ForeignFunctionInterface
             , TemplateHaskell
             , FlexibleContexts #-}

module RustRasterizerExperiment ( RustRasterizerExperiment
                                ) where

import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.State.Class
import Foreign.C.Types
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

data Scene = Cube | Sphere | CornellBox | Head | TorusKnot
             deriving (Eq, Show, Enum, Bounded)

data RustRasterizerExperiment = RustRasterizerExperiment
    { _rrTimes  :: !(BS.BoundedSequence Double)
    , _rrBgType :: !Int
    , _rrScene  :: !Scene
    , _rrMode   :: !Mode
    }

makeLenses ''RustRasterizerExperiment

instance Experiment RustRasterizerExperiment where
    withExperiment f = do f $ RustRasterizerExperiment { _rrTimes  = BS.empty 60
                                                       , _rrBgType = 0
                                                       , _rrScene  = CornellBox
                                                       , _rrMode   = Fill
                                                       }
    experimentName _ = "RustRasterizer"
    experimentDraw fb tick = do
        mode   <- use rrMode
        scene  <- use rrScene
        bgtype <- use rrBgType
        mbtime <- liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                fst <$> timeIt (rastDraw (fromIntegral $ fromEnum mode)
                                         (fromIntegral $ fromEnum scene)
                                         (fromIntegral bgtype)
                                         (realToFrac tick)
                                         (fromIntegral w)
                                         (fromIntegral h)
                                         pvec)
        case mbtime of
            Just time -> rrTimes %= BS.push_ time
            Nothing   -> return ()
    experimentStatusString = do
        RustRasterizerExperiment { .. } <- get
        let avgtime = fromMaybe 1 . median . BS.toList $ _rrTimes
        num_tri <- liftIO $ rastGetMeshTriCnt (fromIntegral $ fromEnum _rrScene)
        return $ printf "%.1fFPS/%.2fms | [B]grnd Type\nSc[e]ne %i/%i: %s (%s) | [M]ode: %s"
                        (1 / avgtime)
                        (avgtime * 1000)
                        (fromEnum _rrScene + 1)
                        (fromEnum (maxBound :: Scene) + 1)
                        (show _rrScene)
                        ( if   num_tri > 1000
                          then printf "%.1fK Tri" (fromIntegral num_tri / 1000 :: Float)
                          else printf "%i Tri" (fromIntegral num_tri :: Int)
                          :: String
                        )
                        (show _rrMode)
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'B -> rrBgType += 1
                    GLFW.Key'E -> rrScene  %= wrapSucc
                    GLFW.Key'M -> rrMode   %= wrapSucc
                    _          -> return ()
            _ -> return ()

foreign import ccall "rast_draw" rastDraw :: CInt       -- Enum RenderMode
                                          -> CInt       -- Enum Scene
                                          -> CInt       -- Background Type Idx
                                          -> CDouble    -- Tick
                                          -> CInt       -- Framebuffer Width
                                          -> CInt       -- Framebuffer Height
                                          -> Ptr Word32 -- Framebuffer Pointer
                                          -> IO ()

foreign import ccall "rast_get_mesh_tri_cnt" rastGetMeshTriCnt :: CInt    -- Enum Scene
                                                               -> IO CInt

