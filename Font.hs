
module Font (withFontTexture, drawText) where

import Data.Word (Word8, Word32)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (ord)
import qualified Foreign.Marshal.Array (withArray)
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU (build2DMipmaps)
import Control.Exception
import Control.Monad

import GLHelpers
import QuadRendering

withFontTexture :: (GL.TextureObject -> IO a) -> IO a
withFontTexture f = do
  traceOnGLError $ Just "withFontTexture begin"
  r <- bracket
    GL.genObjectName
    GL.deleteObjectName
    $ \tex -> do
      -- Font texture
      GL.textureBinding GL.Texture2D GL.$= Just tex
      setTextureFiltering GL.Texture2D TFMinOnly
      -- Convert font grid bitmap image from Word32 list into byte array
      let fontImgArray =
              VU.fromListN (fontGridWdh * fontGridHgt * fontCharWdh * fontCharHgt `div` 8) .
                  concatMap (\x -> map (extractByte x) [0..3]) $ miscFixed6x12Data
                      :: VU.Vector Word8
      -- Extract bits (reversed in byte), store transparent / opaque pixels in square texture
      let fontTex = [toRGBA $ texel x y | y <- [0..fontTexWdh - 1], x <- [0..fontTexWdh - 1]]
           where texel x y = (srcLookup x y .&. (1 `shiftL` (7 - (srcIdx x y `mod` 8))))
                 srcLookup x y | (x < fontImgWdh && y < fontImgHgt) =
                                     fontImgArray VU.! (srcIdx x y `div` 8)
                               | otherwise = 0
                 srcIdx x y = x + y * fontImgWdh
                 toRGBA a = case a of 0 -> 0x0FFFFFF; _ -> 0xFFFFFFFF :: Word32
      Foreign.Marshal.Array.withArray fontTex $ \ptr ->
          -- TODO: Just use GPU MIP-map generation
          GLU.build2DMipmaps GL.Texture2D GL.RGBA'
                             (fromIntegral fontTexWdh)
                             (fromIntegral fontTexWdh)
                             (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
      traceOnGLError $ Just "withFontTexture begin inner"
      f tex
  traceOnGLError $ Just "withFontTexture after cleanup"
  return r

drawText :: GL.TextureObject -> QuadRenderBuffer -> Int -> Int -> Word32 -> String -> IO ()
drawText tex qb x y color str = do
    let charAndPos = filter (\(_, _, c) -> c /= '\n') .
                         scanl (\(x', y', _) a -> if   a == '\n'
                                                  then ((-1)  , y' - 1, a)
                                                  else (x' + 1, y'    , a)
                               ) ((-1), 0, '\n') $ str
    forM_ charAndPos $ \(xc, yc, chr) ->
        let xoffs          = xc * fontCharWdh
            yoffs          = yc * (fontCharHgt - 1)
            idx            = ord chr
            tx             = (idx `mod` fontGridWdh);
            ty             = fontGridHgt - ((idx - (idx `mod` fontGridWdh)) `div` fontGridWdh + 1);
            ftx            = fromIntegral (tx * fontCharWdh) / fromIntegral fontTexWdh;
            fty            = fromIntegral (ty * fontCharHgt) / fromIntegral fontTexWdh;
            fontCharWdhTex = fromIntegral fontCharWdh / fromIntegral fontTexWdh
            fontCharHgtTex = fromIntegral fontCharHgt / fromIntegral fontTexWdh
            channel i      = fromIntegral (extractByte color i) / 255.0
         in drawQuad qb
                     (fromIntegral $ x + xoffs)
                     (fromIntegral $ y + yoffs)
                     (fromIntegral $ x + xoffs + fontCharWdh)
                     (fromIntegral $ y + yoffs + fontCharHgt)
                     1
                     (FCSolid $ RGBA (channel 0) (channel 1) (channel 2) 1)
                     TRSrcAlpha
                     (Just tex)
                     $ QuadUV ftx fty (ftx + fontCharWdhTex) (fty + fontCharHgtTex)

extractByte :: Word32 -> Int -> Word8
extractByte x i = fromIntegral $ (x .&. (0xFF `shiftL` (i * 8))) `shiftR` (i * 8)

-- Bit packed font data for a 16 x 16 charcter grid of 6 x 12 pixel characters
fontGridWdh, fontGridHgt, fontImgWdh, fontImgHgt, fontCharWdh, fontCharHgt, fontTexWdh :: Int
fontGridWdh = 16
fontGridHgt = 16
fontImgWdh  = 96
fontImgHgt  = 192
fontCharWdh = 6
fontCharHgt = 12
fontTexWdh  = 256
miscFixed6x12Data :: [Word32]
miscFixed6x12Data =
    [ 0x00000000, 0x00000000, 0x20080200, 0x00000000, 0x00000000, 0x10080100, 0x711c2772, 0xc7f100c7
    , 0x088f701c, 0x8aa2288a, 0x28ca8828, 0x944889a2, 0x8aa2288a, 0x28aa8028, 0xa2288aa2, 0x8aa2288b
    , 0x289abe28, 0xa2288aa2, 0x711cc77a, 0x287a00c7, 0x222f8aa2, 0x00000008, 0x00000800, 0x00080000
    , 0x5208c252, 0x820000c5, 0x14885014, 0x2104a421, 0x010100a0, 0x00400008, 0x00000050, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00001800, 0x00000000, 0x00000000
    , 0x00000400, 0x00000000, 0x799ee779, 0xc7719ce7, 0x1cc7711c, 0x8aa2288a, 0x0882222a, 0x08822020
    , 0x799ee779, 0xcff320e7, 0x0882203c, 0x08822008, 0x288aa222, 0x088220a2, 0x711cc771, 0xc7711cc7
    , 0x1886611c, 0x00000000, 0x00000080, 0x00000000, 0x512c8520, 0x85200040, 0x14852014, 0x001a4240
    , 0x42400080, 0x00424000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00800000, 0x00000000, 0x711c2772, 0xc77100c7
    , 0x2c84701c, 0x8aa2284a, 0x28caa228, 0x228788a2, 0x8aa2684a, 0x28aa9428, 0xa48488a2, 0x8aa2a8ea
    , 0x28aa8828, 0xa88488a2, 0x8aa2284b, 0x28aa9428, 0xa44489a2, 0x8aa2284a, 0x289aa228, 0x22278aa2
    , 0x711c2772, 0x287200c7, 0x1c248aa2, 0x00000000, 0x00080000, 0x00000000, 0x5208c202, 0x820000c5
    , 0x00805014, 0x2104a401, 0x010100a0, 0x00400008, 0x00000000, 0x00001800, 0x00000000, 0x00000000
    , 0x00000400, 0x00000000, 0x8aa2288a, 0xeffb9c2b, 0x1cc771be, 0x8aa2288a, 0x0882222a, 0x08822020
    , 0x8aa2288a, 0x0882202a, 0x08822020, 0xfbbeeffb, 0xcff320ef, 0x0882203c, 0x8aa2288a, 0x0882202a
    , 0x08822020, 0x8aa2288a, 0x0882222a, 0x08822020, 0x711cc771, 0xeffb9cc7, 0x1cc771be, 0x00000000
    , 0x00000080, 0x00000000, 0x512c8520, 0x85200040, 0x14852014, 0x001a4240, 0x42400080, 0x00424000
    , 0x02000000, 0x00600000, 0x00000000, 0x02000000, 0x00100000, 0x00000000, 0x0300e003, 0x000080a2
    , 0x1ce11028, 0x02000000, 0x00008062, 0x22811014, 0x02008000, 0x00008022, 0x9047780a, 0x02008000
    , 0x00008c26, 0x08255014, 0x0200e003, 0x00008c2e, 0x08a33028, 0x40188730, 0xc701800e, 0x004d5100
    , 0x20048248, 0x8000800e, 0x08024100, 0x10080148, 0x82008007, 0x00044100, 0x00040530, 0x85010000
    , 0x0002c300, 0x00180200, 0x82000000, 0x000c4100, 0x00000000, 0x00000000, 0x00000000, 0x00000200
    , 0x00000000, 0x00000000, 0xa82c8700, 0xe0011c82, 0x8007000a, 0x50928a00, 0x10020282, 0x40080814
    , 0x8b108a00, 0x50020ce2, 0x400a0828, 0x50b88a00, 0x90021280, 0x40caf914, 0xab108700, 0x570212e2
    , 0x400b000a, 0x01120200, 0x10020c42, 0x40080000, 0x020c8000, 0xe3011022, 0x80070000, 0x00000000
    , 0x05500e00, 0x3e000000, 0x00000000, 0x03000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00002080, 0x00020000, 0x00000000, 0x00002080, 0x00010000, 0x00002104, 0x193ce8f1, 0x8f8814a2
    , 0x00802088, 0x2202288a, 0x44512a65, 0x00802008, 0x221c288a, 0x2222aa28, 0x00892008, 0x22a02c8a
    , 0x2152a228, 0x804a2010, 0xfa1eebf1, 0x2f8aa228, 0x80842088, 0x20000000, 0x00000000, 0x00802008
    , 0x20000000, 0x00000000, 0x00802008, 0x00000000, 0x00000000, 0x00002104, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x03001c00, 0x00000000, 0x00000000
    , 0x04000200, 0x00000080, 0x791cef01, 0xc0891ec4, 0x9ca872a2, 0x8aa22802, 0x80882204, 0xa2a822a4
    , 0x8ba0e801, 0x808822c4, 0xa2a822b8, 0x8aa22800, 0x8088222e, 0xa2ac22a4, 0x791ccf01, 0x81f11cc4
    , 0x1c4b23a2, 0x08000810, 0x00808004, 0x00002020, 0x08000820, 0x80800003, 0x000060a0, 0x00000040
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x3e000000, 0x00000000, 0x00000000, 0x00c0011c, 0x219ca881, 0x8f8814c2
    , 0x00400890, 0x22224982, 0x88882a25, 0x00401010, 0x2202aa82, 0x84502a25, 0x00401010, 0x221c2ff2
    , 0x8220a228, 0x00402010, 0x22a0288a, 0x4151a228, 0x00404010, 0x22a2288a, 0x208aa228, 0x80484090
    , 0xfa1ccff1, 0x2f8aa228, 0x00458090, 0x00000000, 0x00000000, 0x00c2011c, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0xf31c2f72, 0xc6891ce8, 0x9c28fa22, 0x4aa22482, 0x89882208, 0xa2288224
    , 0x4aa024ba, 0x81882608, 0xa2298228, 0x4b20e7ab, 0x81f820cf, 0xa22a8230, 0x4aa024ba, 0x81882008
    , 0xa2ac8228, 0x4aa2248a, 0x81882208, 0xa2688324, 0xf31ccf71, 0xc3899cef, 0x9c2882a2, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000030, 0x119ccf31, 0x867108c7
    , 0x08000018, 0x12228448, 0x46888828, 0x00041018, 0xf8028248, 0x20888828, 0x08e22300, 0x900c8148
    , 0xe671042f, 0x08014018, 0x53848048, 0x268a04c8, 0x04e22318, 0x32828849, 0x208a0204, 0x22041000
    , 0x133e8730, 0xc0713ee3, 0x1c000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x20000000
    , 0x00110000, 0x0000c000, 0x72148000, 0x82208066, 0x20066000, 0xaa3e0000, 0x8a200069, 0x10066088
    , 0x29148000, 0x4740800a, 0x10000008, 0x70148000, 0x42400084, 0x08e0033e, 0xa03e8000, 0x4740004a
    , 0x04000008, 0xab148500, 0x8a20082a, 0x04000088, 0x73008500, 0x82200824, 0x02000000, 0x20000500
    , 0x00110800, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000
    , 0xfc000000, 0x80200082, 0x00000000, 0x00000000, 0x80200082, 0x00000000, 0x00000000, 0x8f200082
    , 0x000b51be, 0x003f0000, 0x80200082, 0x80045100, 0x00000000, 0x81200082, 0x00e453b0, 0x00c00f00
    , 0x86fc3ffe, 0x0c8e500c, 0x00000000, 0x88000882, 0x0ce4fb02, 0x00000000, 0x86000882, 0x8044000c
    , 0x0000f003, 0x81000882, 0x004300b0, 0x00000000, 0x80000882, 0x00000000, 0x00000000, 0x80000882
    , 0x00000000, 0x000000fc, 0x80000882, 0x00000000, 0x00400500, 0x00000000, 0x08002000, 0x00800a00
    , 0x00000000, 0x08002000, 0x204405a8, 0x00f800a2, 0x08002000, 0x20848a00, 0x000000a3, 0x08002000
    , 0x3044c589, 0x002000c2, 0x08002000, 0xa084ea03, 0x002080a3, 0xff03e038, 0xb96ec589, 0x00f800c0
    , 0x08020008, 0xc2a88a00, 0x00200c0e, 0x08020008, 0x827805a8, 0x00201208, 0x08020008, 0xe2a80a00
    , 0x00001208, 0x08020008, 0x01680500, 0x00000c88, 0x08020008, 0x00800a00, 0x00000000, 0x08020008
    ]

