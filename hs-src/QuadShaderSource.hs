
{-# LANGUAGE QuasiQuotes #-}

module QuadShaderSource ( vsSrcBasic
                        , fsSrcBasic
                        , fsColOnlySrcBasic
                        ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

-- Shader source for basic vertex and fragment shaders
vsSrcBasic, fsSrcBasic, fsColOnlySrcBasic :: B.ByteString

vsSrcBasic = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                uniform mat4 in_mvp;
                in vec3 in_pos;
                in vec4 in_col;
                in vec2 in_uv;
                out vec4 fs_col;
                out vec2 fs_uv;
                void main()
                {
                    gl_Position = in_mvp * vec4(in_pos, 1.0);
                    fs_col      = in_col;
                    fs_uv       = in_uv;
                }
    |]

fsSrcBasic = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                in vec4 fs_col;
                in vec2 fs_uv;
                uniform sampler2D tex;
                out vec4 frag_color;
                void main()
                {
                   frag_color = fs_col * texture(tex, fs_uv);
                }
    |]

fsColOnlySrcBasic =  TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                in vec4 fs_col;
                out vec4 frag_color;
                void main()
                {
                   frag_color = fs_col;
                }
    |]

