
{-# LANGUAGE   RecordWildCards
             , ForeignFunctionInterface
             , TemplateHaskell
             , BangPatterns
             , LambdaCase
             , FlexibleContexts #-}

module RustGoLExperiment ( RustGoLExperiment
                         ) where

import Control.Monad.IO.Class
import Control.Lens
import Control.Monad
import Control.Monad.State.Class
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Maybe
import Text.Printf
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Storable as VS
import qualified Graphics.UI.GLFW as GLFW

import Experiment
import FrameBuffer
import Timing
import qualified BoundedSequence as BS
import Median
import qualified GoLPatterns as GP
import GLFWHelpers

-- Game of Life

data GoLStats =  GoLStats !Int !Double
                 deriving (Show, Eq)

newtype NumWorkers = NumWorkers { getNumWorkers :: Int }

data RustGoLExperiment = RustGoLExperiment { -- Serialize main / worker thread access to Rust code.
                                             --   Also contains the number of worker threads the
                                             --   Rust code should use
                                             rgolLock :: MVar NumWorkers
                                             -- Statistics from the worker thread
                                           , rgolStats :: MVar GoLStats
                                           }

makeLenses ''RustGoLExperiment

instance Experiment RustGoLExperiment where
    withExperiment f = do setPatternNoLock GP.ark
                          rgolLock  <- newMVar $ NumWorkers 1
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
        nt <- liftIO . readMVar =<< gets rgolLock
        return $ printf "256^2 Grid, %iGs, %.2fms, %iGPS\n[RGAFK] Pattern | Threads [T][t]: %i"
                        ngen
                        (avgtime * 1000)
                        (round $ 1 / avgtime :: Int)
                        (getNumWorkers nt)
    experimentGLFWEvent ev = do
        case ev of
            GLFWEventKey _win k _sc ks mk | ks == GLFW.KeyState'Pressed ->
                case k of
                    GLFW.Key'R -> randomizePattern
                    GLFW.Key'G -> setPattern GP.gun
                    GLFW.Key'A -> setPattern GP.acorn
                    GLFW.Key'F -> setPattern GP.spacefill
                    GLFW.Key'K -> setPattern GP.ark
                    GLFW.Key'T | GLFW.modifierKeysShift mk -> modifyNumWorkers pred
                               | otherwise                 -> modifyNumWorkers succ
                    _          -> return ()
                  where modifyNumWorkers action =
                            (liftIO . flip modifyMVar_
                                (return . NumWorkers . min 16 . max 1 . action . getNumWorkers))
                                    =<< gets rgolLock

            _ -> return ()

-- Worker thread does computation, gets stalled when we draw / modify the grid
golWorker :: MVar NumWorkers -> MVar GoLStats -> IO ()
golWorker lock stats = go (BS.empty 30) (0 :: Int)
    where go !bs !ngen = do
              -- Timed GoL step
              time <- withMVar lock $ \nt ->
                  fst <$> timeIt (golStep . fromIntegral $ getNumWorkers nt)
              -- Update stats and keep going
              let bs' = BS.push_ time bs
              modifyMVar stats $ \_ ->
                  return ( GoLStats ngen (fromMaybe 1 . median . BS.toList $ bs')
                         , ()
                         )
              go bs' (ngen + 1)

setPattern :: (MonadState RustGoLExperiment m, MonadIO m) => [String] -> m ()
setPattern asciiPat =
    gets rgolLock >>= \lock ->
        liftIO . withMVar lock $ \_ ->
            setPatternNoLock asciiPat

-- Set the grid from an ASCII drawing of a GoL pattern
setPatternNoLock :: [String] -> IO ()
setPatternNoLock asciiPat =
    let w = case asciiPat of
                (x:_) -> length x
                _     -> 0
        h = length asciiPat
        v = VS.fromList $ concatMap (map (\case { 'O' -> 1; _ -> 0 })) asciiPat
    in  (VS.length v == w * h) `assert` VS.unsafeWith v $ \pvec ->
            golSetPattern (fromIntegral w) (fromIntegral h) pvec

randomizePattern :: (MonadState RustGoLExperiment m, MonadIO m) => m ()
randomizePattern =
    gets rgolLock >>= \lock ->
        liftIO . withMVar lock $ \_ ->
            golRandomize

foreign import ccall "gol_draw"        golDraw       :: CInt -> CInt -> Ptr Word32 -> IO ()
foreign import ccall "gol_step"        golStep       :: CInt -> IO ()
foreign import ccall "gol_randomize"   golRandomize  :: IO ()
foreign import ccall "gol_set_pattern" golSetPattern :: CInt -> CInt -> Ptr Word8 -> IO ()

