-- Simple examples of using Rasterific

module Rasterific where

  -- JuicyPixels
import Codec.Picture

  -- Rasterific
import Graphics.Rasterific
import Graphics.Rasterific.Texture

-- A simple example of a using Rasterific 

white     = PixelRGBA8 255 255 255 255
drawColor = PixelRGBA8 0 0x86 0xc1 255
recColor  = PixelRGBA8 0xFF 0x53 0x73 255

img = renderDrawing 400 200 white $
          withTexture (uniformTexture drawColor) $ do
             fill $ circle (V2 0 0) 30
             stroke 4 JoinRound (CapRound, CapRound) $
                    circle (V2 400 200) 40
             withTexture (uniformTexture recColor) .
                    fill $ rectangle (V2 100 100) 200 100
