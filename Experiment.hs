
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

-- Existential wrappers for creating / using arbitrary experiments
type WithExperiment e  = forall a. Experiment e => (e -> IO a) -> IO a
data AnyExperiment     = forall e. Experiment e => AnyExperiment e
data AnyWithExperiment = forall e. Experiment e => AnyWithExperiment (WithExperiment e)

class Experiment e where
    withExperiment :: WithExperiment e
    experimentName :: e -> String
    -- statusString :: (MonadState e m, MonadIO m) -> m String
    -- draw
    -- update
    experimentGLFWEvent :: (MonadIO m) => GLFWEvent -> e -> m e
    experimentGLFWEvent _ e = return e

    experimentGLFWEventState :: (MonadIO m, MonadState e m) => GLFWEvent -> m ()
    experimentGLFWEventState _ = return ()

--runExperimentState :: (MonadIO m, Experiment e, MonadState e ms, MonadIO ms) => e -> ms a -> m (e, a)
--runExperimentState f = liftIO 

-- Dummy experiment for initialization etc.
data EmptyExperiment = EmptyExperiment
instance Experiment EmptyExperiment where
    withExperiment f = f EmptyExperiment
    experimentName _ = "EmptyExperiment"

