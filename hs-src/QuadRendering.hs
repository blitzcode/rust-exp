
{-# LANGUAGE   RecordWildCards
             , OverloadedStrings
             , LambdaCase
             , FlexibleContexts
             , BangPatterns
             , ScopedTypeVariables #-}

module QuadRendering ( withQuadRenderer
                     , QuadRenderer
                     , withQuadRenderBuffer
                     , QuadRenderBuffer
                     , drawQuad
                     , gatherRenderStats
                       -- Re-exports from GLHelpers
                     , Transparency(..)
                       -- Re-exports from QuadTypes
                     , RGBA(..)
                     , FillColor(..)
                     , QuadUV(..)
                     ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Mutable as VM
import Data.List
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Except
import Control.DeepSeq
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Text.Printf

import Trace
import GLHelpers
import GLSLHelpers
import QuadShaderSource
import QuadTypes

-- Module for efficient rendering of 2D quad primitives, used for UI elements and texture
-- mapped font rendering
--
-- TODO: We could speed this up quite a bit by using a geometry shader, significantly
--       reducing the amount of vertex and index data we have to generate and write

data QuadRenderer = QuadRenderer
    { -- Vertex / Element Array Buffer Objects and layout
      qrVAO            :: !GL.VertexArrayObject
    , qrVBO            :: !GL.BufferObject
    , qrEBO            :: !GL.BufferObject
    , qrVtxStride      :: !Int
    , qrColStride      :: !Int
    , qrUVStride       :: !Int
    , qrTotalStride    :: !Int
    , qrMaxQuad        :: !Int
    , qrMaxVtx         :: !Int
    , qrMaxTri         :: !Int
    , qrVtxOffset      :: !Int
    , qrColOffset      :: !Int
    , qrUVOffset       :: !Int
      -- Shaders
    , qrShdProgTex     :: !GL.Program
    , qrShdProgColOnly :: !GL.Program
      -- Rendering statistics
    , qrRenderStats    :: !(IORef String)
    }

-- Bind and allocate Vertex / Element Array Buffer Object (VBO / EBO)
bindAllocateDynamicBO :: GL.BufferObject -> GL.BufferTarget -> Int -> IO ()
bindAllocateDynamicBO bo target size = do
    GL.bindBuffer target GL.$= Just bo
    GL.bufferData target GL.$= ( fromIntegral size -- In bytes
                               , nullPtr
                               , GL.StreamDraw -- Dynamic
                               )

setAttribArray :: GL.GLuint
               -> Int
               -> Int
               -> Int
               -> IO GL.AttribLocation
setAttribArray idx attribStride vertexStride offset = do
    -- Specify and enable vertex attribute array
    let attrib = GL.AttribLocation idx
        szf    = sizeOf (0 :: Float)
    GL.vertexAttribPointer attrib GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor
              (fromIntegral attribStride)
              GL.Float
              (fromIntegral $ vertexStride * szf)
              (nullPtr `plusPtr` (offset * szf))
        )
    GL.vertexAttribArray attrib GL.$= GL.Enabled
    return attrib

-- Initialize / clean up all OpenGL resources for our renderer
withQuadRenderer :: Int -> (QuadRenderer -> IO a) -> IO a
withQuadRenderer qrMaxQuad f =
    traceOnGLError (Just "withQuadRenderer begin") >>
    -- Allocate OpenGL objects
    let glBracket bo = bracket GL.genObjectName GL.deleteObjectName bo
     in glBracket $ \qrVAO ->
        glBracket $ \qrVBO ->
        glBracket $ \qrEBO -> do
            qrRenderStats <- newIORef ""
            -- VAO
            GL.bindVertexArrayObject GL.$= Just qrVAO
            -- VBO
            let szf           = sizeOf (0 :: Float)
                qrVtxStride   = 3
                qrColStride   = 4
                qrUVStride    = 2
                qrTotalStride = qrVtxStride + qrColStride + qrUVStride
                qrMaxTri      = qrMaxQuad * 2
                qrMaxVtx      = qrMaxTri * 4
                numfloat      = qrTotalStride * qrMaxVtx
                qrVtxOffset   = 0
                qrColOffset   = qrVtxOffset + qrVtxStride
                qrUVOffset    = qrColOffset + qrColStride
            bindAllocateDynamicBO qrVBO GL.ArrayBuffer $ numfloat * szf
            -- Specify and enable vertex attribute arrays
            vtxAttrib <- setAttribArray 0 qrVtxStride qrTotalStride qrVtxOffset
            colAttrib <- setAttribArray 1 qrColStride qrTotalStride qrColOffset
            uvAttrib  <- setAttribArray 2 qrUVStride  qrTotalStride qrUVOffset
            let attribLocations = [ ("in_pos", vtxAttrib)
                                  , ("in_col", colAttrib)
                                  , ("in_uv" , uvAttrib )
                                  ]
            -- EBO
            let numIdx = qrMaxTri * 3
                szi    = sizeOf(0 :: GL.GLuint)
            bindAllocateDynamicBO qrEBO GL.ElementArrayBuffer $ numIdx * szi
            -- Create, compile and link shaders
            r <- runExceptT . runResourceT $ do
                     qrShdProgTex     <- tryMkShaderResource $
                         mkShaderProgram vsSrcBasic fsSrcBasic attribLocations
                     qrShdProgColOnly <- tryMkShaderResource $
                         mkShaderProgram vsSrcBasic fsColOnlySrcBasic attribLocations
                     -- Initialization done, run inner
                     liftIO $ do
                         disableVAOAndShaders
                         traceOnGLError $ Just "withQuadRenderer begin inner"
                         finally
                             ( f $ QuadRenderer { .. } )
                             ( traceOnGLError $ Just "withQuadRenderer after inner" )
            either (traceAndThrow . printf "withQuadRenderer - Shader init failed:\n%s") return r

-- TODO: Write an Unbox instance for this and switch to an unboxed mutable vector
data QuadRenderAttrib = QuadRenderAttrib
    { qaFillTransparency :: !Transparency
    , qaMaybeTexture     :: !(Maybe GL.TextureObject)
    , qaIndex            :: !Int -- Index into the VBO so we know what indices to generate
                                 -- after sorting by attributes
    , qaDepth            :: !Float -- We store the depth / layer information already in the
                                   -- VBO, but replicate them here so we can sort for transparency
    } deriving (Eq)

-- Back-to-front ordering (transparency) and then sorting to reduce OpenGL state changes
instance Ord QuadRenderAttrib where
    compare a b = let cmpDepth = compare (qaDepth            b) (qaDepth            a)
                      cmpTex   = compare (qaMaybeTexture     a) (qaMaybeTexture     b)
                      cmpTrans = compare (qaFillTransparency a) (qaFillTransparency b)
                  in  case () of
                          _ | cmpDepth /= EQ -> cmpDepth -- Sort by depth first
                            | cmpTex   /= EQ -> cmpTex   -- Sort by texture at the same depth
                            | otherwise      -> cmpTrans -- Finally by transparency

data QuadRenderBuffer = QuadRenderBuffer
    { qbQR      :: !QuadRenderer
    , qbNumQuad :: !(IORef Int)
    , qbAttribs :: !(VM.IOVector  QuadRenderAttrib)
    , qbVBOMap  :: !(VSM.IOVector GL.GLfloat      )
    }

-- Prepare data structures and render when inner exits. This is meant to be called once or
-- more per-frame. Runs its inner inside the base monad. Takes width and height so it
-- knows how to setup the orthographic projection for the shader
withQuadRenderBuffer :: forall a m. (MonadBaseControl IO m, MonadIO m)
                     => QuadRenderer
                     -> Int
                     -> Int
                     -> (QuadRenderBuffer -> m a)
                     -> m (Maybe a) -- We return Nothing if mapping fails
withQuadRenderBuffer qbQR@(QuadRenderer { .. }) w h f = do
    -- Map. If this function is nested inside a withQuadRenderBuffer with the same QuadRenderer,
    -- the mapping operation will fail as OpenGL does not allow two concurrent mappings. Hence,
    -- no need to check for this explicitly
    r <- control $ \run -> liftIO $
        let bindVBO = GL.bindBuffer GL.ArrayBuffer GL.$= Just qrVBO
            -- TODO: We could use glMapBufferRange instead and safe some work for
            --       partially filled buffers, get asynchronous transfers etc.
        in  bindVBO >> GL.withMappedBuffer -- VBO
                GL.ArrayBuffer
                GL.WriteOnly
                ( \ptrVBO -> newForeignPtr_ ptrVBO >>= \fpVBO ->
                      let numfloat = qrMaxVtx * qrTotalStride
                          qbVBOMap = VSM.unsafeFromForeignPtr0 fpVBO numfloat
                      in  do qbNumQuad <- newIORef 0
                             qbAttribs <- VM.new qrMaxQuad
                             finally
                                 ( run $ do -- Run in outer base monad
                                       let qb = QuadRenderBuffer { .. }
                                       r <- f qb
                                       return $ Just (r, qb)
                                 )
                                 bindVBO -- Make sure we rebind our VBO, otherwise
                                         -- unmapping might fail if the inner
                                         -- modified the bound buffer objects
                )
                ( \mf -> do traceS TLError $
                                "withQuadRenderBuffer - VBO mapping failure: " ++ show mf
                            -- Looks like since the 1.0.0.0 change in monad-control we need
                            -- some type annotations for this to work
                            run $ (return Nothing :: m (Maybe (a, QuadRenderBuffer)))
                )
    case r of
        Nothing       -> return Nothing
        Just (ra, qb) -> liftIO $ do
            -- VBO has been successfully mapped, filled and unmapped, attributes have been
            -- collected as well, proceed to render
            dr <- drawRenderBuffer qb w h
            return $ if dr then Just ra else Nothing

-- Internal function to draw the contents of a render buffer once we're done filling it
drawRenderBuffer :: QuadRenderBuffer -> Int -> Int -> IO Bool
drawRenderBuffer (QuadRenderBuffer { .. }) w h = do
    let QuadRenderer { .. } = qbQR
    GL.bindVertexArrayObject GL.$= Just qrVAO
    numQuad <- readIORef qbNumQuad
    attribs <- sortAttributes qbAttribs numQuad
    eboSucc <- fillEBO qrMaxTri attribs
    if not eboSucc
      then return False
      else do
        -- Setup
        forM_ [qrShdProgTex, qrShdProgColOnly] $ \shdProg -> do
            GL.currentProgram GL.$= Just shdProg
            setOrtho2DProjMatrix shdProg "in_mvp" w h
        -- Texture, use first TU
        GL.currentProgram  GL.$= Just qrShdProgTex
        (GL.get $ GL.uniformLocation qrShdProgTex "tex") >>= \loc ->
            GL.uniform loc GL.$= GL.Index1 (0 :: GL.GLint)
        GL.activeTexture   GL.$= GL.TextureUnit 0
        -- Setup some initial state and build corresponding attribute record
        GL.currentProgram              GL.$= Just qrShdProgColOnly
        GL.textureBinding GL.Texture2D GL.$= Nothing
        setTransparency TRNone
        let initialState = QuadRenderAttrib TRNone Nothing 0 0.0
        -- Draw all quads
        foldM_
            ( \(oldA, i) a -> do
                  let newA   = head a
                      numIdx = length a * 6 -- TODO: Slow, just output this during the first pass
                  -- Modify OpenGL state which changed between old / new rendering attributes
                  case (qaMaybeTexture oldA, qaMaybeTexture newA) of
                     (Just oldTex, Just newTex) ->
                         when (oldTex /= newTex) $
                             GL.textureBinding GL.Texture2D GL.$= Just newTex
                     (Nothing, Just newTex) -> do
                         GL.currentProgram              GL.$= Just qrShdProgTex
                         GL.textureBinding GL.Texture2D GL.$= Just newTex
                     (Just _, Nothing) ->
                         GL.currentProgram GL.$= Just qrShdProgColOnly
                     (Nothing, Nothing) ->
                         return ()
                  when (qaFillTransparency oldA /= qaFillTransparency newA) .
                      setTransparency $ qaFillTransparency newA
                  -- Draw all quads in the current attribute group as two triangles
                  let szi = sizeOf(0 :: GL.GLuint)
                   in GL.drawElements GL.Triangles
                                      (fromIntegral numIdx)
                                      GL.UnsignedInt
                                      $ nullPtr `plusPtr` (i * szi)
                  return (newA, i + numIdx)
            )
            (initialState, 0)
            attribs
        -- Store statistics inside QuadRenderer record. Need to make sure the string has
        -- been fully generated, no dependency on the rendering data should be kept
        let statString = printf "Last drawRenderBuffer drawElementCalls: %i · numQuad: %i"
                                (length attribs)
                                numQuad
         in statString `deepseq` writeIORef qrRenderStats statString
        -- Done
        disableVAOAndShaders
        return True

gatherRenderStats :: QuadRenderer -> IO String
gatherRenderStats = readIORef . qrRenderStats

-- Sort and group attributes (for transparency and reduced state changes)
sortAttributes :: VM.IOVector QuadRenderAttrib -> Int -> IO [[QuadRenderAttrib]]
sortAttributes attribs numQuad =
    groupBy (\a b -> compare a b == EQ) . -- Group by state into single draw calls. We
                                          --   use the compare instance used for state
                                          --   sorting so we only break groups on
                                          --   relevant changes
    sort . V.toList                       -- TODO: Sort mutable vector in-place with
                                          --       vector-algorithms?
    <$> ( V.unsafeFreeze                  -- Can only convert immutable vector to a list
              . VM.unsafeTake numQuad     -- Drop undefined elements
              $ attribs
        )

-- Build EBO from state sorted attributes. This benchmarked slightly faster than doing
-- drawing in a single pass with ad-hoc index buffer building
fillEBO :: Int -> [[QuadRenderAttrib]] -> IO Bool -- Return false on mapping failure
fillEBO maxTri attribs = do
    GL.withMappedBuffer -- EBO, this assumes a matching VAO has been bound
      GL.ElementArrayBuffer
      GL.WriteOnly
      ( \ptrEBO -> newForeignPtr_ ptrEBO >>= \fpEBO ->
          let !numIdx = 3 * maxTri
              !eboMap = VSM.unsafeFromForeignPtr0 fpEBO numIdx :: VSM.IOVector GL.GLuint
          in  do foldM_ -- Fold over draw call groups
                   ( \r a -> do
                       n <- foldM
                         ( \gr ga -> do -- Fold over quads in group
                             -- Write index data to the mapped element array buffer
                             let !eboOffs = gr * 6
                                 !vboOffs = qaIndex ga
                                 uw       = VSM.unsafeWrite eboMap
                              in -- Unrolled version of
                                 -- forM_ (zip [eboOffs..] [0, 1, 2, 0, 2, 3]) $ \(i, e) ->
                                 --     VSM.write eboMap i (fromIntegral $ e + vboOffs)
                                 do uw (eboOffs + 0) . fromIntegral $ vboOffs + 0
                                    uw (eboOffs + 1) . fromIntegral $ vboOffs + 1
                                    uw (eboOffs + 2) . fromIntegral $ vboOffs + 2
                                    uw (eboOffs + 3) . fromIntegral $ vboOffs + 0
                                    uw (eboOffs + 4) . fromIntegral $ vboOffs + 2
                                    uw (eboOffs + 5) . fromIntegral $ vboOffs + 3
                             return $! gr + 1 -- Next six EBO entries
                         ) r a
                       return n
                   ) 0 attribs
                 return True
      )
      ( \mf -> do traceS TLError $ "drawRenderBuffer - EBO mapping failure: " ++ show mf
                  return False
      )

-- Record all data to render the specified quad into the passed render buffer
drawQuad :: QuadRenderBuffer
         -> Float -> Float -> Float -> Float
         -> Float
         -> FillColor
         -> Transparency
         -> Maybe GL.TextureObject
         -> QuadUV
         -> IO ()
drawQuad (QuadRenderBuffer { .. })
         !x1 !y1 !x2 !y2
         !qaDepth
         col
         qaFillTransparency
         qaMaybeTexture
         uv = do
    let QuadRenderer { .. } = qbQR -- Bring buffer layout information into scope
    -- Are we at capacity?
    numQuad <- readIORef qbNumQuad
    if numQuad == qrMaxQuad
        then traceT TLError "drawQuad - QuadRenderBuffer overflow, dropping quad"
        else do
          -- Write vertex data to our mapped attribute buffers
          --
          -- TODO: Could use a hashmap to reuse vertices between adjacent quads
          --
          -- TODO: The code we're using is an unrolled version of this:
          --
          -- let (pos', cols, texs) = paramToPosColUV x1 y1 x2 y2 col
          --     vboOffs            = numQuad * 4
          -- forM_ (zip4 [vboOffs..] pos' cols texs) $
          --     \(i, (x, y), RGBA r g b a, (u, v)) ->
          --         forM_ (zip [0..] [x, y, (-qaDepth), r, g, b, a, u, v]) $
          --             \(offs, f) -> VSM.write qbVBOMap (i * qrTotalStride + offs) $ realToFrac f
          --
          -- Would be nice to find a more elegant yet still fast version
          --
          let !vtxBase = numQuad * 4 * qrTotalStride
              !vtx0    = vtxBase + (qrTotalStride * 0)
              !vtx1    = vtxBase + (qrTotalStride * 1)
              !vtx2    = vtxBase + (qrTotalStride * 2)
              !vtx3    = vtxBase + (qrTotalStride * 3)
              !( RGBA !r0 !g0 !b0 !a0
               , RGBA !r1 !g1 !b1 !a1
               , RGBA !r2 !g2 !b2 !a2
               , RGBA !r3 !g3 !b3 !a3
               ) = case col of FCWhite                 -> let c = RGBA 1 1 1 1 in (c, c, c, c)
                               FCBlack                 -> let c = RGBA 0 0 0 1 in (c, c, c, c)
                               FCSolid c               -> (c, c, c, c)
                               FCBottomTopGradient b t -> (b, b, t, t)
                               FCLeftRightGradient l r -> (l, r, r, l)
              !(!u0, !v0, !u1, !v1) =
                  case uv of QuadUVDefault          -> (0, 0, 1, 1)
                             QuadUV u0' v0' u1' v1' -> (u0', v0', u1', v1')
              uw =  VSM.unsafeWrite qbVBOMap
           in do -- Vertex 0
                 uw (vtx0 + 0) $ realToFrac x1         -- X
                 uw (vtx0 + 1) $ realToFrac y1         -- Y
                 uw (vtx0 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx0 + 3) $ realToFrac r0         -- R
                 uw (vtx0 + 4) $ realToFrac g0         -- G
                 uw (vtx0 + 5) $ realToFrac b0         -- B
                 uw (vtx0 + 6) $ realToFrac a0         -- A
                 uw (vtx0 + 7) $ realToFrac u0         -- U
                 uw (vtx0 + 8) $ realToFrac v0         -- V
                 -- Vertex 1
                 uw (vtx1 + 0) $ realToFrac x2         -- X
                 uw (vtx1 + 1) $ realToFrac y1         -- Y
                 uw (vtx1 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx1 + 3) $ realToFrac r1         -- R
                 uw (vtx1 + 4) $ realToFrac g1         -- G
                 uw (vtx1 + 5) $ realToFrac b1         -- B
                 uw (vtx1 + 6) $ realToFrac a1         -- A
                 uw (vtx1 + 7) $ realToFrac u1         -- U
                 uw (vtx1 + 8) $ realToFrac v0         -- V
                 -- Vertex 2
                 uw (vtx2 + 0) $ realToFrac x2         -- X
                 uw (vtx2 + 1) $ realToFrac y2         -- Y
                 uw (vtx2 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx2 + 3) $ realToFrac r2         -- R
                 uw (vtx2 + 4) $ realToFrac g2         -- G
                 uw (vtx2 + 5) $ realToFrac b2         -- B
                 uw (vtx2 + 6) $ realToFrac a2         -- A
                 uw (vtx2 + 7) $ realToFrac u1         -- U
                 uw (vtx2 + 8) $ realToFrac v1         -- V
                 -- Vertex 3
                 uw (vtx3 + 0) $ realToFrac x1         -- X
                 uw (vtx3 + 1) $ realToFrac y2         -- Y
                 uw (vtx3 + 2) $ realToFrac (-qaDepth) -- Z
                 uw (vtx3 + 3) $ realToFrac r3         -- R
                 uw (vtx3 + 4) $ realToFrac g3         -- G
                 uw (vtx3 + 5) $ realToFrac b3         -- B
                 uw (vtx3 + 6) $ realToFrac a3         -- A
                 uw (vtx3 + 7) $ realToFrac u0         -- U
                 uw (vtx3 + 8) $ realToFrac v1         -- V
          -- Write rendering attributes (need to be strict since it's not an unboxed vector)
          VM.unsafeWrite qbAttribs numQuad $! QuadRenderAttrib { qaIndex = numQuad * 4, .. }
          -- One more quad
          modifyIORef' qbNumQuad (+ 1)

