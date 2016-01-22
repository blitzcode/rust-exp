
{-# LANGUAGE RecordWildCards, TemplateHaskell, LambdaCase, FlexibleContexts #-}

module App ( AppState(..)
           , AppEnv(..)
           , AppT
           , runAppT
           , run
             -- Export to silence warnings
           , aeFontTexture
           , aeQR
           ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
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

data AppState = AppState { _asCurTick          :: !Double
                         , _asLastEscPress     :: !Double
                         , _asFrameTimes       :: BS.BoundedSequence Double
                         , _asFrameIdx         :: !Int
                         }

data AppEnv = AppEnv { _aeWindow           :: GLFW.Window
                     , _aeGLFWEventsQueue  :: TQueue GLFWEvent
                     , _aeFontTexture      :: GL.TextureObject
                     , _aeFB               :: FrameBuffer
                     , _aeQR               :: QuadRenderer
                     }

makeLenses ''AppState
makeLenses ''AppEnv

-- Our application runs in a reader / state / IO transformer stack

type AppT m = StateT AppState (ReaderT AppEnv m)
type AppIO = AppT IO

runAppT :: Monad m => AppState -> AppEnv -> AppT m a -> m a
runAppT s e f = flip runReaderT e . flip evalStateT s $ f

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
                -- Mode / scaling switch is a render settings change
                --GLFW.Key'Minus  -> asMode %= wrapPred >> onRenderSettingsChage
                --GLFW.Key'Equal  -> asMode %= wrapSucc >> onRenderSettingsChage
                GLFW.Key'S      -> view aeFB >>= \fb -> liftIO $ saveFrameBufferToPNG fb .
                                     map (\c -> if c `elem` ['/', '\\', ':', ' '] then '-' else c)
                                       . printf "Screenshot-%s.png" =<< show <$> getZonedTime
                _               -> return ()
        GLFWEventFramebufferSize _win _w _h -> resize
        _ -> return ()

-- Handle changes in window and frame buffer size
resize :: AppIO ()
resize = do
    window <- view aeWindow
    fb     <- view aeFB
    liftIO $ do (w, h) <- GLFW.getFramebufferSize window
                setupViewport w h
                resizeFrameBuffer fb w h
    onRenderSettingsChage

{-
-- Move through an enumeration, but wrap around when hitting the end
wrapSucc, wrapPred :: (Enum a, Bounded a, Eq a) => a -> a
wrapSucc a | a == maxBound = minBound
           | otherwise     = succ a
wrapPred a | a == minBound = maxBound
           | otherwise     = pred a
-}

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
                              0                (fromIntegral h - 24)
                              (fromIntegral w) (fromIntegral h)
                              2
                              FCBlack
                              (TRBlend 0.5)
                              Nothing
                              QuadUVDefault
            ftStr <- updateAndReturnFrameTimes
            (fbWdh, fbHgt) <- liftIO $ getFrameBufferDim _aeFB
            liftIO . drawTextWithShadow _aeFontTexture qb 3 (h - 12) $
                printf "[S]creenshot | 2x[ESC] Exit | %ix%i\n%s"
                       fbWdh
                       fbHgt
                       ftStr

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
     in return $ printf "%.2fFPS/%.1fms (Worst: %.2f, Best: %.2f)"
                        (1.0 / fdMean)
                        (fdMean  * 1000)
                        (1.0 / fdWorst)
                        (1.0 / fdBest)

drawTextWithShadow :: GL.TextureObject -> QuadRenderBuffer -> Int -> Int -> String -> IO ()
drawTextWithShadow tex qb x y str = do
    drawText tex qb (x + 1) (y - 1) 0x00000000 str
    drawText tex qb  x       y      0x0000FF00 str

onRenderSettingsChage :: MonadState AppState m => m ()
onRenderSettingsChage = do
    -- Reset frame time measurements and frame index when the rendering settings have
    -- changed. Also cancel any outstanding screen shot requests
    asFrameTimes     %= BS.clear
    asFrameIdx       .= 0

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- view aeWindow
    resize
    liftIO $ GLFW.swapInterval 1
    -- Main loop
    let loop = do
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
          flip unless loop =<< liftIO (GLFW.windowShouldClose window)
     in loop

