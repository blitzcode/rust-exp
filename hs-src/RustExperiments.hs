
{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}

module RustExperiments ( SomeExperiment1
                       , SomeExperiment2
                       , RustSineExperiment
                       ) where

-- import Control.Concurrent.Async
import Control.Concurrent.MVar
import Foreign.C.Types

import Experiment

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
    withExperiment f = do rustHelloWord 42
                          f RustSineExperiment
    experimentName _ = "RustSine"

foreign import ccall "rust_hello_word" rustHelloWord :: CInt -> IO ()

