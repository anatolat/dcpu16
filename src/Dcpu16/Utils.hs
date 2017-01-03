{-# LANGUAGE ScopedTypeVariables #-}

module Dcpu16.Utils 
    ( bytesToWords
    , mvectorToByteString
    ) where

import Data.Word
import Data.Bits
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Foreign.Storable as FS
import qualified Foreign.ForeignPtr as FP

toWord16 :: Word8 -> Word8 -> Word16
toWord16 lsb msb = fromIntegral lsb .|. (fromIntegral msb `shiftL` 8)

bytesToWords :: [Word8] -> [Word16]
bytesToWords = go
    where go [] = []
          go (x:y:xs) = toWord16 x y : go xs
          

sizeOfElem :: forall a m. (FS.Storable a) => MV.MVector m a -> Int
sizeOfElem vec = FS.sizeOf (undefined :: a)

mvectorToByteString :: (FS.Storable a) => MV.IOVector a -> BS.ByteString
mvectorToByteString vec
  = BS.fromForeignPtr (FP.castForeignPtr fptr) (scale off) (scale len)  where
    (fptr, off, len) = MV.unsafeToForeignPtr vec
    scale = (* sizeOfElem vec)