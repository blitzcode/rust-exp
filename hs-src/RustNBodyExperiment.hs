
{-# LANGUAGE   RecordWildCards
             , ForeignFunctionInterface
             , TemplateHaskell
             , BangPatterns
             , FlexibleContexts #-}

module RustNBodyExperiment ( RustNBodyExperiment
                           ) where

import Control.Monad.IO.Class
import Control.Lens
import Control.Monad
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

-- N-Body Simulation

data RustNBodyExperiment = RustNBodyExperiment
    { _rnbNumSteps :: !Int
    , _rnbTimes    :: !(BS.BoundedSequence Double)
    }

makeLenses ''RustNBodyExperiment

instance Experiment RustNBodyExperiment where
    withExperiment f = do liftIO $ nbStableOrbits 1000 0.5 30.0
                          f $ RustNBodyExperiment { _rnbNumSteps = 0, _rnbTimes = BS.empty 30 }
    experimentName _ = "RustNBody"
    experimentDraw fb _tick = do
        time <- fst <$> liftIO (timeIt nbStep) -- Simulate first
        void . liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                nbDraw (fromIntegral w) (fromIntegral h) pvec
        rnbNumSteps += 1
        rnbTimes    %= BS.push_ time
    experimentStatusString = do
        RustNBodyExperiment { .. } <- get
        let avgtime = fromMaybe 1 . median . BS.toList $ _rnbTimes
        return $ printf "%iS, %.2fms, %iSPS\n"
                        _rnbNumSteps
                        (avgtime * 1000)
                        (round $ 1 / avgtime :: Int)
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'A -> return ()
                    _          -> return ()
            _ -> return ()

foreign import ccall "nb_draw"          nbDraw         :: CInt -> CInt -> Ptr Word32 -> IO ()
foreign import ccall "nb_step"          nbStep         :: IO ()
foreign import ccall "nb_random_disk"   nbRandomDisk   :: CInt -> IO ()
foreign import ccall "nb_stable_orbits" nbStableOrbits :: CInt -> CFloat -> CFloat -> IO ()

