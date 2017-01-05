{-# LANGUAGE ScopedTypeVariables #-}

module Dcpu16.Utils 
    ( module Data.Vector.Storable.ByteString
    , writeVectorToFile
    ) where

import qualified Data.Vector.Storable as SV
import qualified Data.ByteString as BS
import qualified Foreign.Storable as FS
import Data.Vector.Storable.ByteString
          
writeVectorToFile :: (FS.Storable a) => SV.Vector a -> FilePath -> IO ()
writeVectorToFile vec filePath = BS.writeFile filePath bs
    where bs = vectorToByteString vec