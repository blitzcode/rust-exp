
{-# LANGUAGE   ForeignFunctionInterface
             , TemplateHaskell
             , FlexibleContexts #-}

module RustSineExperiment ( RustSineExperiment
                          ) where

import Control.Monad.IO.Class
import Control.Lens
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

-- Simple 2D scrolling sine waves

data RustSineExperiment = RustSineExperiment { _rseTime :: !(BS.BoundedSequence Double) }

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

