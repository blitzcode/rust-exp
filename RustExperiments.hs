
{-# LANGUAGE RecordWildCards #-}

module RustExperiments ( SomeExperiment1
                       , SomeExperiment2
                       ) where

-- import Control.Concurrent.Async
import Control.Concurrent.MVar

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

