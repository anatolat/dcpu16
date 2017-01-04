{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Dcpu16.Emulator (runEmulatorLoop) where

import Dcpu16.Cpu
import Dcpu16.Video
import Dcpu16.Utils

import qualified Data.Vector.Storable.Mutable as MV
import qualified SDL
import qualified Data.ByteString as BS
import Data.IORef
import Control.Monad

loadBinProg :: CpuState -> FilePath -> IO ()
loadBinProg cpu path = do  
    bs <- BS.readFile path
    writeMemoryBlock cpu $ bytesToWords $ BS.unpack bs


updateInput :: CpuState -> IORef Int -> [SDL.EventPayload] -> IO ()
updateInput cpu pointerRef events = do
    let codes = concatMap 
            (\case SDL.KeyboardEvent e | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                            case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                                SDL.KeycodeLeft  -> [1]
                                SDL.KeycodeRight -> [2]
                                SDL.KeycodeUp    -> [3]
                                SDL.KeycodeDown  -> [4]
                                _ -> []
                   _ -> []) 
            events

    forM_ codes $ \code -> do
        addr <- (+ inputStart) <$> readIORef pointerRef
        old <- readMemory cpu addr
        when (old == 0) $ do
            writeMemory cpu addr $ fromIntegral code
            modifyIORef pointerRef (\x -> (x + 1) `mod` inputMaxCount)
    
    
runEmulatorLoop :: IO ()
runEmulatorLoop = do
    SDL.initialize [SDL.InitVideo]

    let windowSize = SDL.V2 (fromIntegral $ screenWidth * screenScale) (fromIntegral $ screenHeight * screenScale)
    let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
    window <- SDL.createWindow "DCPU-16" windowConfig
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming $
        SDL.V2 (fromIntegral screenWidth) (fromIntegral screenHeight)

    cpu <- newCpu
    loadBinProg cpu "tests/pacman-1.1.bin"

    screenBuf <- MV.new (screenWidth * screenHeight)
    counterRef :: IORef Int <- newIORef 0
    keypointerRef :: IORef Int <- newIORef 0
    
    let loop = do
            events <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` events

            counterValue <- readIORef counterRef

            updateInput cpu keypointerRef events
            run cpu

            when (counterValue `mod` 1000 == 0) $ do
                putStrLn $ "Step " ++ show (counterValue + 1)
                updateScreen cpu screenBuf

                let screenBs = mvectorToByteString screenBuf
                SDL.updateTexture texture Nothing screenBs (fromIntegral $ screenWidth * 4)

                SDL.copy renderer texture Nothing Nothing
                SDL.present renderer

            modifyIORef counterRef (+1)
            unless quit loop
    loop

    SDL.destroyWindow window
    SDL.quit

