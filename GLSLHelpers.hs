
{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module GLSLHelpers ( mkShaderProgram
                   , compileShaderProgram
                   , tryMkShaderResource
                   , setTextureShader
                   , setOrtho2DProjMatrix
                   ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLR
import qualified Data.ByteString as B
import Data.Either
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Foreign.Marshal.Array

import GLHelpers

-- GLSL shaders and support functions

mkShaderProgram :: B.ByteString
                -> B.ByteString
                -> [(String, GL.AttribLocation)]
                -> IO (Either String GL.Program)
mkShaderProgram vsSrc fsSrc attribLocations =
    -- Only delete the program on error
    bracketOnError GL.createProgram GL.deleteObjectName $ \shdProg -> do
        compileShaderProgram vsSrc fsSrc attribLocations shdProg >>=
            \case Left err -> do -- The bracket only deletes in case of an exception,
                                 -- still need to delete manually in case of a monadic error
                                 GL.deleteObjectName shdProg
                                 return $ Left err
                  Right () -> return $ Right shdProg

compileShaderProgram :: B.ByteString
                     -> B.ByteString
                     -> [(String, GL.AttribLocation)]
                     -> GL.Program
                     -> IO (Either String ())
compileShaderProgram vsSrc fsSrc attribLocations shdProg =
    -- Delete the shaders (don't need them after linking)
    bracket (GL.createShader GL.VertexShader  ) (GL.deleteObjectName) $ \shdVtx  ->
    bracket (GL.createShader GL.FragmentShader) (GL.deleteObjectName) $ \shdFrag ->
        runExceptT $ do
            compile shdVtx  vsSrc
            compile shdFrag fsSrc
            liftIO $ GL.attachShader shdProg shdVtx >> GL.attachShader shdProg shdFrag
            -- Need to specify attribute locations before we link
            liftIO . forM_ attribLocations $
                \(name, loc) -> GL.attribLocation shdProg name GL.$= loc
            link shdProg
            liftIO $ GL.detachShader shdProg shdVtx >> GL.detachShader shdProg shdFrag
            liftIO . traceOnGLError $ Just "compileShaderProgram end"
    -- Compile and link helpers
    where compile shd src = do
              liftIO $ do GL.shaderSourceBS shd GL.$= src
                          GL.compileShader  shd
              success <- liftIO $ GL.get $ GL.compileStatus shd
              unless success $ do
                  errLog <- liftIO . GL.get $ GL.shaderInfoLog shd
                  throwError errLog
          link prog = do
              liftIO $ GL.linkProgram prog
              success <- liftIO . GL.get $ GL.linkStatus prog
              unless success $ do
                  errLog <- liftIO $ GL.get $ GL.programInfoLog prog
                  throwError errLog

-- Helper for mkShaderProgam, guaranteeing deallocation through ResourceT and
-- reports errors through MonadError
tryMkShaderResource :: (MonadError String m, MonadIO m, MonadResource m)
                    => IO (Either String GL.Program)
                    -> m GL.Program
tryMkShaderResource f =
    allocate f (GL.deleteObjectNames . rights . (: [])) >>= (either throwError return . snd)

setTextureShader :: GL.BindableTextureTarget t
                 => GL.TextureObject
                 -> t
                 -> Int
                 -> GL.Program
                 -> String
                 -> IO ()
setTextureShader tex target tu prog uname = do
    (GL.get $ GL.uniformLocation prog uname) >>= \loc ->
        GL.uniform loc       GL.$= GL.Index1 (fromIntegral tu :: GL.GLint)
    GL.activeTexture         GL.$= GL.TextureUnit (fromIntegral tu)
    GL.textureBinding target GL.$= Just tex

setOrtho2DProjMatrix :: GL.Program -> String -> Int -> Int -> IO ()
setOrtho2DProjMatrix prog uniform w h = do
    GL.UniformLocation loc <- GL.get $ GL.uniformLocation prog uniform
    let ortho2D = [ 2 / fromIntegral w, 0, 0, -1,
                    0, 2 / fromIntegral h, 0, -1,
                    0, 0, (-2) / 1000, -1, 
                    0, 0, 0, 1
                  ] :: [GL.GLfloat]
    withArray ortho2D $ \ptr -> GLR.glUniformMatrix4fv loc 1 1 {- transpose -} ptr

