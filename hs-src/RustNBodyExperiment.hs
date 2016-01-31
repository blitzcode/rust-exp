
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
    , _rnbTimeStep :: !Double
    }

makeLenses ''RustNBodyExperiment

instance Experiment RustNBodyExperiment where
    withExperiment f = do liftIO $ nbStableOrbits 1000 0.5 30.0
                          f $ RustNBodyExperiment { _rnbNumSteps = 0
                                                  , _rnbTimes    = BS.empty 30
                                                  , _rnbTimeStep = 0.01
                                                  }
    experimentName _ = "RustNBody"
    experimentDraw fb _tick = do
        dt <- use rnbTimeStep
        -- Simulate first
        let theta = 0.5 :: Float
        time <- fst <$> liftIO (timeIt . nbStepBarnesHut (realToFrac theta) . realToFrac $ dt)
        void . liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                nbDraw (fromIntegral w) (fromIntegral h) pvec
        rnbNumSteps += 1
        rnbTimes    %= BS.push_ time
    experimentStatusString = do
        RustNBodyExperiment { .. } <- get
        let avgtime = fromMaybe 1 . median . BS.toList $ _rnbTimes
        np <- liftIO (fromIntegral <$> nbNumParticles :: IO Int)
        return $ printf ( "%i Steps, %.2fms, %.1f Steps/Second\n" ++
                         "%s Particles | [QWE] Setup Particles | " ++
                         "Time Step [T][t]: %.4f"
                        )
                        _rnbNumSteps
                        (avgtime * 1000)
                        (1 / avgtime)
                        ( if   np > 999
                          then show (np `div` 1000) ++ "K"
                          else show np
                        )
                        _rnbTimeStep
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'Q -> liftIO $ nbStableOrbits 20000 0.5 30.0
                    GLFW.Key'W -> liftIO $ nbRandomDisk   20000
                    GLFW.Key'E -> liftIO $ nbStableOrbits 5 5.0 40.0
                    GLFW.Key'T | GLFW.modifierKeysShift mk -> rnbTimeStep //= 2
                               | otherwise                 -> rnbTimeStep  *= 2
                    _          -> return ()
            _ -> return ()

foreign import ccall "nb_draw"             nbDraw            :: CInt -> CInt -> Ptr Word32 -> IO ()
foreign import ccall "nb_step_brute_force" _nbStepBruteForce :: CFloat -> IO ()
foreign import ccall "nb_step_barnes_hut"  nbStepBarnesHut   :: CFloat -> CFloat -> IO ()
foreign import ccall "nb_random_disk"      nbRandomDisk      :: CInt -> IO ()
foreign import ccall "nb_stable_orbits"    nbStableOrbits    :: CInt -> CFloat -> CFloat -> IO ()
foreign import ccall "nb_num_particles"    nbNumParticles    :: IO CInt

