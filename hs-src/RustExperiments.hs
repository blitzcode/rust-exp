
{-# LANGUAGE RecordWildCards, ForeignFunctionInterface, TemplateHaskell, BangPatterns #-}

module RustExperiments ( RustSineExperiment
                       , RustGoLExperiment
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

import Experiment
import FrameBuffer
import Timing
import qualified BoundedSequence as BS
import Median

--
-- Simple 2D scrolling sine waves
--

data RustSineExperiment = RustSineExperiment { _rseTime :: !(BS.BoundedSequence Double)
                                             }

makeLenses ''RustSineExperiment

instance Experiment RustSineExperiment where
    withExperiment f = f $ RustSineExperiment (BS.empty 30)
    experimentName _ = "RustSine"
    experimentDraw fb tick = do
        mbtime <- liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                fst <$> timeIt (sineScroller (fromIntegral w) (fromIntegral h) pvec tick)
        case mbtime of
            Just time -> rseTime %= BS.push_ time
            Nothing   -> return ()
    experimentStatusString = do
        times <- use $ rseTime.to BS.toList
        return . printf "%.2fms" . (* 1000) . fromMaybe 1 $ median times

foreign import ccall "sine_scroller" sineScroller :: CInt -> CInt -> Ptr Word32 -> Double -> IO ()

--
-- Game of Life
--

data GoLStats =  GoLStats !Int !Double
                 deriving (Show, Eq)

data RustGoLExperiment = RustGoLExperiment { -- Serialize main / worker thread access to Rust code
                                             rgolLock :: MVar ()
                                             -- Statistics from the worker thread
                                           , rgolStats :: MVar GoLStats
                                           }

instance Experiment RustGoLExperiment where
    withExperiment f = do golRandomize
                          rgolLock  <- newMVar ()
                          rgolStats <- newMVar $ GoLStats 0 1
                          withAsync (golWorker rgolLock rgolStats) $ \_ ->
                              f $ RustGoLExperiment { .. }
    experimentName _ = "RustGoL"
    experimentDraw fb _tick =
        gets rgolLock >>= \lock ->
            liftIO . void $ withMVar lock $ \_ ->
                fillFrameBuffer fb $ \w h vec ->
                    VSM.unsafeWith vec $ \pvec ->
                        golDraw (fromIntegral w) (fromIntegral h) pvec
    experimentStatusString = do
        GoLStats ngen avgtime <- liftIO . readMVar =<< gets rgolStats
        return $ printf "256^2 Grid, %i Gens, %.2fms, %iGPS"
                        ngen
                        (avgtime * 1000)
                        (round $ 1 / avgtime :: Int)

-- Worker thread does computation, gets stalled when we draw / modify the grid
golWorker :: MVar () -> MVar GoLStats -> IO ()
golWorker lock stats = go (BS.empty 30) (0 :: Int)
    where go !bs !ngen = do
              -- Timed GoL step
              time <- withMVar lock $ \_ ->
                  fst <$> timeIt golStep
              -- Update stats and keep going
              let bs' = BS.push_ time bs
              modifyMVar stats $ \_ ->
                  return ( GoLStats ngen (fromMaybe 1 . median . BS.toList $ bs')
                         , ()
                         )
              go bs' (ngen + 1)

foreign import ccall "gol_draw" golDraw :: CInt -> CInt -> Ptr Word32 -> IO ()
foreign import ccall "gol_step" golStep :: IO ()
foreign import ccall "gol_randomize" golRandomize :: IO ()

