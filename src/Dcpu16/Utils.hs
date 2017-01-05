{-# LANGUAGE ScopedTypeVariables #-}

module Dcpu16.Utils 
    ( module Data.Vector.Storable.ByteString
    , bytesToWords
    , writeVectorToFile
    ) where

import Data.Word
import Data.Bits
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString as BS
import qualified Foreign.Storable as FS
import Data.Vector.Storable.ByteString

toWord16 :: Word8 -> Word8 -> Word16
toWord16 lsb msb = fromIntegral lsb .|. (fromIntegral msb `shiftL` 8)

bytesToWords :: [Word8] -> [Word16]
bytesToWords = go
    where go [] = []
          go (x:y:xs) = toWord16 x y : go xs
          
writeVectorToFile :: (FS.Storable a) => SV.Vector a -> FilePath -> IO ()
writeVectorToFile vec filePath = BS.writeFile filePath bs
    where bs = vectorToByteString vec