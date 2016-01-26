
{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}

module RustExperiments ( SomeExperiment1
                       , SomeExperiment2
                       , RustSineExperiment
                       ) where

-- import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import qualified Data.Vector.Storable.Mutable as VSM

import Experiment
import FrameBuffer

data SomeExperiment1 = SomeExperiment1 { se1MVar    :: !(MVar String)
                                       , se1SomeInt :: !Int
                                       }

instance Experiment SomeExperiment1 where
    withExperiment f = do se1MVar <- newEmptyMVar
                          f SomeExperiment1 { se1SomeInt = 0, .. }
    experimentName _ = "SomeExperiment1"

data SomeExperiment2 = SomeExperiment2 { se2MVar    :: !(MVar String)
                                       , se2SomeInt :: !Int
                                       }

instance Experiment SomeExperiment2 where
    withExperiment f = do se2MVar <- newEmptyMVar
                          f SomeExperiment2 { se2SomeInt = 0, .. }
    experimentName _ = "SomeExperiment2"

data RustSineExperiment = RustSineExperiment

instance Experiment RustSineExperiment where
    withExperiment f = f RustSineExperiment
    experimentName _ = "RustSine"
    experimentDraw fb tick =
        liftIO . void . fillFrameBuffer fb $ \w h vec ->
            VSM.unsafeWith vec $ \pvec ->
                sineScroller (fromIntegral w) (fromIntegral h) pvec tick

foreign import ccall "sine_scroller" sineScroller :: CInt -> CInt -> Ptr Word32 -> Double -> IO ()

