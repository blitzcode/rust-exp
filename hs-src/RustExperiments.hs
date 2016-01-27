
{-# LANGUAGE RecordWildCards, ForeignFunctionInterface, TemplateHaskell #-}

module RustExperiments ( RustSineExperiment
                       , RustGoLExperiment
                       ) where

import Control.Monad.IO.Class
import Control.Lens
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Text.Printf
import qualified Data.Vector.Storable.Mutable as VSM

import Experiment
import FrameBuffer
import Timing
import qualified BoundedSequence as BS

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
        return . printf "%.2fms" . (* 1000) $ sum times / fromIntegral (length times)

foreign import ccall "sine_scroller" sineScroller :: CInt -> CInt -> Ptr Word32 -> Double -> IO ()

--
-- Game of Life
--

data RustGoLExperiment = RustGoLExperiment {
                                           }

instance Experiment RustGoLExperiment where
    withExperiment f = f $ RustGoLExperiment
    experimentName _ = "RustGoL"
    experimentDraw fb tick = do
        {-
        liftIO . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                return ()
        -}
        return ()
    experimentStatusString = return ""

