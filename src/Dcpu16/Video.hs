{-# LANGUAGE ScopedTypeVariables #-}
module Dcpu16.Video where

import Dcpu16.Cpu
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV 
import Data.Bits
import Data.Word
import Control.Monad

data SpriteMode = SpriteMode { width :: Int, height :: Int, bitsPerPixel :: Int }

spriteModes :: [SpriteMode]
spriteModes = [ SpriteMode 8 8 4
              , SpriteMode 16 8 2
              , SpriteMode 8 16 2
              , SpriteMode 16 16 1]

pallete :: SV.Vector Word32
pallete = SV.fromList 
            [ 0x000000FF, 0x1B2632FF, 0x493C2BFF, 0x2F484EFF
            , 0x005784FF, 0xBE2633FF, 0x44891AFF, 0xA46422FF
            , 0x31A2F2FF, 0xE06F8BFF, 0xEB8931FF, 0x9D9D9DFF
            , 0xA3CE27FF, 0xB2DCEFFF, 0xFFE26BFF, 0xFFFFFFFF]

writeTexturePixel :: MV.IOVector Word32 -> Int -> Int -> Word16 -> IO ()
writeTexturePixel screen x y pixel = do
    let color = pallete SV.! fromIntegral pixel
    MV.write screen (y * screenWidth + x) color

drawBgPixel :: CpuState -> MV.IOVector Word32 -> Int -> Int -> IO ()
drawBgPixel cpu screen x y = do
    let addr = gfxStart + x `div` 3 + (y `div` 3) * (screenWidth `div` 3)
    w <- readMemory cpu addr
    let c1 = w `shiftR` 12
    let c2 = (w `shiftR` 8) .&. 0xf
    let idx = x `mod` 3 + (y `mod` 3) * 3
    let b = w .&. (0x80 `shiftR` idx)
    writeTexturePixel screen x y (if b /= 0 then c1 else c2)

drawBg :: CpuState -> MV.IOVector Word32 -> IO ()
drawBg cpu screen = do
    let yx = [(y, x) | y <- [0..screenHeight-1], x <- [0..screenWidth-2]]
    forM_ yx (\(y, x) -> drawBgPixel cpu screen x y)

drawSprites :: CpuState -> MV.IOVector Word32 -> IO ()
drawSprites cpu screen = forM_ [0..spriteCount-1] $ drawSprite cpu screen

drawSprite :: CpuState -> MV.IOVector Word32 -> Int -> IO ()    
drawSprite cpu screen index = do
    let addr = spritesStart + index * 2
    w1 :: Int <- fromIntegral <$> readMemory cpu addr
    w2 :: Int <- fromIntegral <$> readMemory cpu (addr + 1)
    let (sx, sy) = ((w1 `shiftR` 8) - 64, (w1 .&. 0xFF) - 64)
    let (modeIndex, color) = (w2 `shiftR` 14, (w2 `shiftR` 10) .&. 0xf)
    let dataAddr = gfxStart + (2 * (w2 .&. 0x3FF))
    let (SpriteMode width height bitsPerPixel) = spriteModes !! modeIndex

    forM_ [(y, x) | y <- [0..height-1], x <- [0..width-1]] $ \(y, x) -> do
        let xy = y * width + x
        let pixelAddr = dataAddr + xy * bitsPerPixel `div` 16
        w <- readMemory cpu pixelAddr
        let offs = 16 - bitsPerPixel - (xy * bitsPerPixel) `mod` 16
        let col = (w `shiftR` offs) .&. ((1 `shiftL` bitsPerPixel) - 1)

        let transparent = if modeIndex == 3 then col == 0 else fromIntegral col == color
        unless transparent $ do
            let (rx, ry) = (sx + x, sy + y)
            when (rx >= 0 && ry >= 0 && rx < screenWidth && ry < screenHeight) $
                writeTexturePixel screen rx ry col

updateScreen :: CpuState -> MV.IOVector Word32 -> IO ()
updateScreen cpu screen = do 
    drawBg cpu screen
    drawSprites cpu screen