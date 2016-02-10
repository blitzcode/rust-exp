
module GLFWHelpers ( withWindow
                   , GLFWEvent(..)
                   , highDPIScaleFactor
                   ) where

import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GL as GLR

-- Various utility functions related to GLFW

withWindow :: Int -> Int -> Bool -> String -> TQueue GLFWEvent -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h srgb title tq =
    bracket
        ( do GLFW.setErrorCallback . Just $ errorCallback tq
             True <- GLFW.init
             -- GLFW.windowHint $ GLFW.WindowHint'Samples 4
             -- GLFW.windowHint $ GLFW.WindowHint'Decorated False
             GLFW.windowHint $ GLFW.WindowHint'Resizable True
             when srgb . GLFW.windowHint $ GLFW.WindowHint'sRGBCapable True
             modernOpenGL
             Just window <- GLFW.createWindow w h title Nothing Nothing
             registerCallbacks window tq
             GLFW.makeContextCurrent $ Just window
             when srgb $ GLR.glEnable GLR.GL_FRAMEBUFFER_SRGB
             return window
        )
        ( \window -> do GLFW.destroyWindow window
                        GLFW.terminate
        )

-- >2.1, no backwards compatibility on OS X
-- http://www.glfw.org/faq.html#how-do-i-create-an-opengl-30-context
modernOpenGL :: IO ()
modernOpenGL = do
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3

highDPIScaleFactor :: GLFW.Window -> IO Double
highDPIScaleFactor win = do
    (scWdh, _) <- GLFW.getWindowSize win
    (pxWdh, _) <- GLFW.getFramebufferSize win
    return $ fromIntegral pxWdh / fromIntegral scWdh

-- Convert GLFW callbacks into events delivered to a queue

data GLFWEvent = GLFWEventError
                     !GLFW.Error
                     !String
               | GLFWEventKey
                     !GLFW.Window
                     !GLFW.Key
                     !Int
                     !GLFW.KeyState
                     !GLFW.ModifierKeys
               | GLFWEventWindowSize
                     !GLFW.Window
                     !Int
                     !Int
               | GLFWEventFramebufferSize
                     !GLFW.Window
                     !Int
                     !Int
               | GLFWEventMouseButton
                     !GLFW.Window
                     !GLFW.MouseButton
                     !GLFW.MouseButtonState
                     !GLFW.ModifierKeys
               | GLFWEventCursorPos
                     !GLFW.Window
                     !Double
                     !Double
               | GLFWEventScroll
                     !GLFW.Window
                     !Double
                     !Double

errorCallback :: TQueue GLFWEvent -> GLFW.Error -> String -> IO ()
errorCallback tq e s = atomically . writeTQueue tq $ GLFWEventError e s

keyCallback :: TQueue GLFWEvent
            -> GLFW.Window
            -> GLFW.Key
            -> Int
            -> GLFW.KeyState
            -> GLFW.ModifierKeys -> IO ()
keyCallback tq win k sc ka mk = atomically . writeTQueue tq $ GLFWEventKey win k sc ka mk

windowSizeCallback :: TQueue GLFWEvent -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback tq win w h = atomically . writeTQueue tq $ GLFWEventWindowSize win w h

framebufferSizeCallback :: TQueue GLFWEvent -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback tq win w h =
    atomically . writeTQueue tq $ GLFWEventFramebufferSize win w h

mouseButtonCallback :: TQueue GLFWEvent
                    -> GLFW.Window
                    -> GLFW.MouseButton
                    -> GLFW.MouseButtonState
                    -> GLFW.ModifierKeys
                    -> IO ()
mouseButtonCallback tq win bttn st mk =
    atomically . writeTQueue tq $ GLFWEventMouseButton win bttn st mk

cursorPosCallback :: TQueue GLFWEvent -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback tq win x y = atomically . writeTQueue tq $ GLFWEventCursorPos win x y

scrollCallback :: TQueue GLFWEvent -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tq win x y = atomically . writeTQueue tq $ GLFWEventScroll win x y

registerCallbacks :: GLFW.Window -> TQueue GLFWEvent -> IO ()
registerCallbacks window tq = do
    GLFW.setKeyCallback             window . Just $ keyCallback tq
    GLFW.setWindowSizeCallback      window . Just $ windowSizeCallback tq
    GLFW.setFramebufferSizeCallback window . Just $ framebufferSizeCallback tq
    GLFW.setMouseButtonCallback     window . Just $ mouseButtonCallback tq
    GLFW.setCursorPosCallback       window . Just $ cursorPosCallback tq
    GLFW.setScrollCallback          window . Just $ scrollCallback tq

