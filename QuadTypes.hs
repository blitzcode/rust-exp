
module QuadTypes ( RGBA(..)
                 , FillColor(..)
                 , QuadUV(..)
                 ) where

-- Types shared between different modules generating / processing / storing quads for
-- OpenGL rendering. Put in their own module to reduce logical and compile time dependency
-- between them

data RGBA = RGBA {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 {-# UNPACK #-} !Float
                 deriving (Eq, Show)

data FillColor = FCWhite
               | FCBlack
               | FCSolid !RGBA
               | FCBottomTopGradient !RGBA !RGBA
               | FCLeftRightGradient !RGBA !RGBA
               deriving (Eq, Show)

data QuadUV = QuadUVDefault
            | QuadUV {-# UNPACK #-} !Float -- UV Bottom Left
                     {-# UNPACK #-} !Float
                     {-# UNPACK #-} !Float -- UV Top Right
                     {-# UNPACK #-} !Float
              deriving (Eq, Show)

