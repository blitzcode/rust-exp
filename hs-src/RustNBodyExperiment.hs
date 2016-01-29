
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
import Control.Concurrent.MVar
import Control.Concurrent.Async
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

data NBStats = NBStats !Int !Double
               deriving (Show, Eq)

data RustNBodyExperiment = RustNBodyExperiment
    { rnbLock  :: MVar ()      -- Serialize main / worker thread access to Rust code
    , rnbStats :: MVar NBStats -- Statistics from the worker thread
    }

makeLenses ''RustNBodyExperiment

instance Experiment RustNBodyExperiment where
    withExperiment f = do rnbLock  <- newMVar ()
                          rnbStats <- newMVar $ NBStats 0 1
                          --withAsync (nbWorker rnbLock rnbStats) $ \_ ->
                          f $ RustNBodyExperiment { .. }
    experimentName _ = "RustNBody"
    experimentDraw fb _tick =
        gets rnbLock >>= \lock ->
            liftIO . void $ withMVar lock $ \_ ->
                fillFrameBuffer fb $ \w h vec ->
                    VSM.unsafeWith vec $ \pvec -> do
                        nbStep
                        nbDraw (fromIntegral w) (fromIntegral h) pvec
    experimentStatusString = do
        NBStats nsteps avgtime <- liftIO . readMVar =<< gets rnbStats
        return $ printf "%i Steps, %.2fms, %iSPS\n"
                        nsteps
                        (avgtime * 1000)
                        (round $ 1 / avgtime :: Int)
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'A -> return ()
                    _          -> return ()
            _ -> return ()

-- Worker thread does computation, gets stalled when we draw / setup the particles
nbWorker :: MVar () -> MVar NBStats -> IO ()
nbWorker lock stats = go (BS.empty 30) (0 :: Int)
    where go !bs !nsteps = do
              -- Timed NBody step
              time <- withMVar lock $ \_ ->
                  fst <$> timeIt nbStep
              -- Update stats and keep going
              let bs' = BS.push_ time bs
              modifyMVar stats $ \_ ->
                  return ( NBStats nsteps (fromMaybe 1 . median . BS.toList $ bs')
                         , ()
                         )
              go bs' (nsteps + 1)

foreign import ccall "nb_draw" nbDraw :: CInt -> CInt -> Ptr Word32 -> IO ()
foreign import ccall "nb_step" nbStep :: IO ()

