
{-# LANGUAGE   RecordWildCards
             , TemplateHaskell
             , LambdaCase
             , FlexibleContexts
             , RankNTypes
             , TypeFamilies #-}

module App ( AppState(..)
           , AppEnv(..)
           , AnyExperiment(..)
           , AppT
           , run
             -- Export to silence warnings
           , aeFontTexture
           , aeQR
           ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.STM
import Control.Monad.Trans.Control
import Control.Concurrent.STM.TQueue
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import Data.Time

import GLFWHelpers
import GLHelpers
import Timing
import Trace
import Font
import FrameBuffer
import QuadRendering
import qualified BoundedSequence as BS
import Experiment

data AppState = AppState { _asCurTick      :: !Double
                         , _asLastEscPress :: !Double
                         , _asFrameTimes   :: BS.BoundedSequence Double
                         , _asFrameIdx     :: !Int
                         , _asExperiment   :: AnyExperiment
                         }

data AppEnv = AppEnv { _aeWindow          :: GLFW.Window
                     , _aeGLFWEventsQueue :: TQueue GLFWEvent
                     , _aeFontTexture     :: GL.TextureObject
                     , _aeFB              :: FrameBuffer
                     , _aeQR              :: QuadRenderer
                     , _aeExperiments     :: [AnyWithExperiment]
                     }

makeLenses ''AppState
makeLenses ''AppEnv

-- Our application runs in a reader / state / either / IO transformer stack
data ExpResult = ExpNext | ExpPrev | ExpExit
                 deriving (Show, Eq, Enum)
type AppT m = EitherT ExpResult (StateT AppState (ReaderT AppEnv m))
type AppIO = AppT IO

processAllEvents :: MonadIO m => TQueue a -> (a -> m ()) -> m ()
processAllEvents tq processEvent =
    (liftIO . atomically $ tryReadTQueue tq) >>= \case
        Just e -> processEvent e >> processAllEvents tq processEvent
        _      -> return ()

processGLFWEvent :: GLFWEvent -> AppIO ()
processGLFWEvent ev =
    case ev of
        GLFWEventError e s -> do
           window <- view aeWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey win k _sc ks _mk | ks == GLFW.KeyState'Pressed ->
            case k of
                GLFW.Key'Escape -> do
                    lastPress <- use asLastEscPress
                    tick      <- use asCurTick
                    -- Only close when ESC has been pressed twice quickly
                    when (tick - lastPress < 0.5) .
                        liftIO $ GLFW.setWindowShouldClose win True
                    asLastEscPress .= tick
                GLFW.Key'S     -> view aeFB >>= \fb -> liftIO $ saveFrameBufferToPNG fb .
                                    map (\c -> if c `elem` ['/', '\\', ':', ' '] then '-' else c)
                                      . printf "Screenshot-%s.png" =<< show <$> getZonedTime
                                  -- Exit and switch to a different experiment
                GLFW.Key'Minus -> onRenderSettingsChage >> left ExpPrev
                GLFW.Key'Equal -> onRenderSettingsChage >> left ExpNext
                _              -> return ()
        GLFWEventFramebufferSize _win _w _h -> resize
        _ -> return ()

-- Handle changes in window and frame buffer size
resize :: (MonadReader AppEnv m, MonadState AppState m, MonadIO m) => m ()
resize = do
    window <- view aeWindow
    fb     <- view aeFB
    liftIO $ do (w, h) <- GLFW.getFramebufferSize window
                setupViewport w h
                resizeFrameBuffer fb w h
    onRenderSettingsChage

onRenderSettingsChage :: MonadState AppState m => m ()
onRenderSettingsChage = do
    -- Reset frame time measurements and frame index when the rendering settings have changed
    asFrameTimes %= BS.clear
    asFrameIdx   .= 0

draw :: AppIO ()
draw = do
    AppEnv   { .. } <- ask
    AppState { .. } <- get
    -- Clear
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 1 0 1 1 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal

    void . fillFrameBuffer _aeFB $ \_w _h _fbVec ->
        return ()

    -- Render everything quad based
    (liftIO $ GLFW.getFramebufferSize _aeWindow) >>= \(w, h) ->
        void . withQuadRenderBuffer _aeQR w h $ \qb -> do
            -- Draw frame buffer contents
            liftIO $ drawFrameBuffer _aeFB qb 0 0 (fromIntegral w) (fromIntegral h)
            -- FPS counter and mode display
            liftIO $ drawQuad qb
                              0                (fromIntegral h - 36)
                              (fromIntegral w) (fromIntegral h)
                              2
                              FCBlack
                              (TRBlend 0.5)
                              Nothing
                              QuadUVDefault
            ftStr <- updateAndReturnFrameTimes
            (fbWdh, fbHgt) <- liftIO $ getFrameBufferDim _aeFB
            liftIO . drawTextWithShadow _aeFontTexture qb 3 (h - 12) $
                printf "2x[ESC] Exit | [S]creenshot | %ix%i | %s\nExperiment %i of %i [-][=]: %s"
                       fbWdh
                       fbHgt
                       ftStr
                       (0 :: Int)
                       (0 :: Int)
                       "Name"

updateAndReturnFrameTimes :: MonadState AppState m => m String
updateAndReturnFrameTimes = do
    frameTimes <- use $ asFrameTimes.to BS.toList
    curTick    <- use asCurTick
    asFrameTimes %= BS.push_ curTick
    let frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
        goFD prev (x:xs) = (prev - x) : goFD x xs
        goFD _ []        = []
        fdMean           = sum frameDeltas / (fromIntegral $ length frameDeltas)
        fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
        fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
     in return $ printf "%.2fFPS/%.1fms (L: %.2f, H: %.2f)"
                        (1.0 / fdMean)
                        (fdMean  * 1000)
                        (1.0 / fdWorst)
                        (1.0 / fdBest)

drawTextWithShadow :: GL.TextureObject -> QuadRenderBuffer -> Int -> Int -> String -> IO ()
drawTextWithShadow tex qb x y str = do
    drawText tex qb (x + 1) (y - 1) 0x00000000 str
    drawText tex qb  x       y      0x0000FF00 str

run :: AppEnv -> AppState -> IO ()
run env st =
    -- Setup state, reader, OpenGL / GLFW and enter loop
    flip runReaderT env . flip evalStateT st $ do
        resize
        liftIO $ GLFW.swapInterval 1
        experimentLoop 0
  where -- Initialize / shutdown / switch experiment
        experimentLoop :: Int -> StateT AppState (ReaderT AppEnv IO) ()
        experimentLoop expIdx = do
            -- We use the either to break out of the current experiment, and either exit
            -- or switch to a different one
            r <- runEitherT $ do
                -- Because of the existential type we have to call withExperiment from
                -- inside the lambda where we pattern match it out of the AnyWithExperiment.
                -- Also use monad-control to bring our stack across the IO of withExperiment
                curExp <- (!! expIdx) <$> view aeExperiments
                control $ \runMonad ->
                    (\(AnyWithExperiment withExperiment') ->
                        liftIO $ withExperiment' (runMonad . withExperimentInner)
                    ) curExp
            numExp <- length <$> view aeExperiments
            -- Exit or keep running with a different experiment?
            case r of
                Left ExpNext -> experimentLoop $ wrapExpIdx (expIdx + 1) numExp 
                Left ExpPrev -> experimentLoop $ wrapExpIdx (expIdx - 1) numExp 
                Left ExpExit -> return ()
                Right ()     -> return ()
              where wrapExpIdx idx numExp | idx < 0       = numExp - 1
                                          | idx >= numExp = 0
                                          | otherwise     = idx
        -- Experiment setup complete, store state and enter main loop 
        withExperimentInner :: Experiment e => e -> AppIO ()
        withExperimentInner expState = do
            liftIO . traceS TLInfo $ "Switching to experiment: " ++ (experimentName expState)
            asExperiment .= AnyExperiment expState
            mainLoop
        -- Main loop
        mainLoop :: AppIO ()
        mainLoop = do
            window <- view aeWindow
            asCurTick <~ liftIO getTick
            tqGLFW <- view aeGLFWEventsQueue
            processAllEvents tqGLFW processGLFWEvent
            -- GLFW / OpenGL
            draw
            liftIO $ {-# SCC swapAndPoll #-} do
                -- GL.flush
                -- GL.finish
                GLFW.swapBuffers window
                GLFW.pollEvents
                traceOnGLError $ Just "main loop"
            -- Drop the first three frame deltas, they are often outliers
            use asFrameIdx >>= \idx -> when (idx < 3) (asFrameTimes %= BS.clear)
            asFrameIdx += 1
            -- Done?
            flip unless mainLoop =<< liftIO (GLFW.windowShouldClose window)

            {-
            exp11 <- use asExperiment
            let exp11b :: forall e. Experiment e => e
                exp11b = (\(AnyExperiment e) -> e) exp11
            exp11 <- use asExperiment
            let callWithExp :: AnyExperiment -> (forall e. Experiment e => e -> AppIO ()) -> AppIO ()
                callWithExp (AnyExperiment e) f = f e
             in callWithExp exp11 (liftIO . putStrLn . experimentName)
            -}
