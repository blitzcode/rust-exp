
{-# LANGUAGE ExistentialQuantification, RankNTypes, FlexibleContexts #-}

module Experiment ( Experiment(..)
                  , AnyExperiment(..)
                  , AnyWithExperiment(..)
                  , WithExperiment
                  , EmptyExperiment
                  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Class

import GLFWHelpers (GLFWEvent)
import FrameBuffer (FrameBuffer)

-- Existential wrappers for creating / using arbitrary experiments
type WithExperiment e  = forall a. Experiment e => (e -> IO a) -> IO a
data AnyExperiment     = forall e. Experiment e => AnyExperiment e
data AnyWithExperiment = forall e. Experiment e => AnyWithExperiment (WithExperiment e)

class Experiment e where
    -- Create experiment state and allow experiment to perform initialization and cleanup
    withExperiment :: WithExperiment e
    -- UI name of the experiment
    experimentName :: e -> String
    experimentName _ = "<Experiment>"
    -- UI status string, displaying current state, statistics, keybindings etc.
    experimentStatusString :: (MonadState e m, MonadIO m) => m String
    experimentStatusString = return ""
    -- Allow the experiment to draw into the framebuffer
    experimentDraw :: (MonadState e m, MonadIO m) => FrameBuffer -> Double -> m ()
    experimentDraw _fb _tick = return ()
    -- Allow the experiment to respond to keyboard / mouse events
    experimentGLFWEvent :: (MonadIO m, MonadState e m) => GLFWEvent -> m ()
    experimentGLFWEvent _ = return ()

-- Dummy experiment for initialization etc.
data EmptyExperiment = EmptyExperiment
instance Experiment EmptyExperiment where
    withExperiment f = f EmptyExperiment
    experimentName _ = "EmptyExperiment"

