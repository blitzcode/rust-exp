
{-# LANGUAGE   TemplateHaskell
             , LambdaCase
             , RankNTypes #-}

module AppDefs where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Concurrent.STM.TQueue
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import GLFWHelpers
import FrameBuffer
import QuadRendering
import qualified BoundedSequence as BS
import Experiment

-- Some definitions and utilities split out from the App module

data AppState = AppState { _asCurTick        :: !Double
                         , _asLastEscPress   :: !Double
                         , _asFrameTimes     :: !(BS.BoundedSequence Double)
                         , _asFrameIdx       :: !Int
                         , _asVSync          :: !Bool
                         , _asExperiment     :: !AnyExperiment
                         , _asExperimentDesc :: !String
                         }

data AppEnv = AppEnv { _aeWindow          :: !GLFW.Window
                     , _aeGLFWEventsQueue :: !(TQueue GLFWEvent)
                     , _aeFontTexture     :: !GL.TextureObject
                     , _aeFB              :: !FrameBuffer
                     , _aeQR              :: !QuadRenderer
                     , _aeExperiments     :: ![AnyWithExperiment]
                     }

makeLenses ''AppState
makeLenses ''AppEnv

-- Our application runs in a reader / state / either / IO transformer stack
data ExpResult = ExpNext | ExpPrev | ExpExit
                 deriving (Show, Eq, Enum)
type AppT m = EitherT ExpResult (StateT AppState (ReaderT AppEnv m))
type AppIO = AppT IO

-- Run a computation in the State monad with the current experiment as its state. Store the
-- final state back into ours. Note that we can't write this with the 'zoom' combinator
-- from lens as we can't define a lens for an existential type
runExperimentState :: (forall e m. (Experiment e, MonadIO m, MonadState e m) => m a) -> AppIO a
runExperimentState f =
    use asExperiment >>= \case
        (AnyExperiment e) -> do
            (r, e') <- liftIO . flip runStateT e $ f
            asExperiment .= AnyExperiment e'
            return r

