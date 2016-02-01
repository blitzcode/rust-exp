
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

-- Software Rasterization

data RustRasterizerExperiment = RustRasterizerExperiment
    { _rrTimes  :: !(BS.BoundedSequence Double)
    , _rrBgType :: !Int
    }

makeLenses ''RustRasterizerExperiment

instance Experiment RustRasterizerExperiment where
    withExperiment f = do f $ RustRasterizerExperiment { _rrTimes  = BS.empty 30
                                                       , _rrBgType = 0
                                                       }
    experimentName _ = "RustRasterizer"
    experimentDraw fb tick = do
        bgtype <- use rrBgType
        mbtime <- liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                fst <$> timeIt (rastDraw (fromIntegral bgtype)
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
        return $ printf "%.1fFPS/%.2fms | [B]grnd Type"
                        (1 / avgtime)
                        (avgtime * 1000)
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'B -> rrBgType += 1
                    _          -> return ()
            _ -> return ()

foreign import ccall "rast_draw" rastDraw :: CInt -> CFloat -> CInt -> CInt -> Ptr Word32 -> IO ()

