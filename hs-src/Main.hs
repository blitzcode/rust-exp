
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Main (main) where

import GHC.Conc (getNumProcessors)
import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.STM.TQueue
import Data.List
import qualified System.Info as SI

import App
import Trace
import GLFWHelpers
import GLHelpers
import Timing
import Font
import FrameBuffer
import QuadRendering
import qualified BoundedSequence as BS
import Experiment
import RustSineExperiment
import RustGoLExperiment

runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

traceSystemInfo :: IO ()
traceSystemInfo = do
    cpus <- GHC.Conc.getNumProcessors
    traceS TLInfo =<<
        ( (++) . concat . intersperse " · " $
             [ "System - OS: " ++ SI.os
             , "Arch: " ++ SI.arch
             , "CPUs: " ++ show cpus
             , concat [ "Compiler: "
                      , SI.compilerName
                      , " / "
                      , show SI.compilerVersion
                      ]
             ]
        )
        <$> (("\n" ++) <$> getGLStrings)
    -- mapM_ (traceS TLInfo) =<< getGLExtensionList

main :: IO ()
main = do
    runOnAllCores
    withTrace Nothing True False True TLInfo $ do
      _aeGLFWEventsQueue <- newTQueueIO
      let w = 512
          h = 512
       in withWindow w h "Viewer" _aeGLFWEventsQueue $ \_aeWindow ->
          withFontTexture $ \_aeFontTexture ->
          withFrameBuffer w h LowQualityDownscaling $ \_aeFB ->
          withQuadRenderer 4096 $ \_aeQR ->
          (withExperiment :: WithExperiment EmptyExperiment) $ \emptyExperiment -> do
            traceSystemInfo
            _asCurTick <- getTick
            let _aeExperiments =
                    [ AnyWithExperiment (withExperiment :: WithExperiment RustSineExperiment)
                    , AnyWithExperiment (withExperiment :: WithExperiment RustGoLExperiment )
                    ]
                ae = AppEnv { .. }
                as = AppState { _asLastEscPress   = -1
                              , _asFrameTimes     = BS.empty 60 -- Average over last N FPS
                              , _asFrameIdx       = 0
                              , _asExperiment     = AnyExperiment emptyExperiment
                              , _asExperimentDesc = ""
                              , ..
                              }
             in run ae as

