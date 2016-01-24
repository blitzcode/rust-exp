
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Experiment ( Experiment(..)
                  , AnyExperiment(..)
                  , AnyWithExperiment(..)
                  , WithExperiment
                  , EmptyExperiment
                  ) where

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
    -- processGLFWEvent :: GLFWEvent -> m ()

--runExperimentState :: (MonadIO m, Experiment e, MonadState e ms, MonadIO ms) => e -> ms a -> m (e, a)
--runExperimentState f = liftIO 

-- Dummy experiment for initialization etc.
data EmptyExperiment = EmptyExperiment
instance Experiment EmptyExperiment where
    withExperiment f = f EmptyExperiment
    experimentName _ = "EmptyExperiment"

