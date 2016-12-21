{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16)
import Data.Bits


screenWidth = 128
screenHeight = 96

memorySize = 0x10000

data Reg = RegA
         | RegB
         | RegC
         | RegX
         | RegY
         | RegZ
         | RegI
         | RegJ
         | RegPC
         | RegSP
         | RegEx
         deriving (Show, Bounded, Enum)

regCount = fromEnum (maxBound :: Reg) + 1

data CpuState = CpuState { memory :: MV.IOVector Word16
                          , regs :: MV.IOVector Word16
                          }


newState :: IO CpuState
newState = do
    mem <- MV.new memorySize
    regs <- MV.new regCount
    return $ CpuState {memory = mem, regs = regs }

readMemory :: CpuState -> Int -> IO Word16
readMemory CpuState {memory = memory} addr = MV.read memory addr

writeMemory :: CpuState -> Int -> Word16 -> IO ()
writeMemory CpuState {memory = memory} addr value = MV.write memory addr value

readRegister :: CpuState -> Reg -> IO Word16
readRegister CpuState {regs = regs} reg = MV.read regs (fromEnum reg)

writeRegister :: CpuState -> Reg -> Word16 -> IO ()
writeRegister CpuState {regs = regs} reg value = MV.write regs (fromEnum reg) value

readPC :: CpuState -> IO Int
readPC cpu = readRegister cpu RegPC >>= return . fromIntegral

stepPC :: CpuState -> IO Word16
stepPC cpu = do
    addr <- readRegister cpu RegPC
    result <- readMemory cpu (fromIntegral addr)
    writeRegister cpu RegPC (addr + 1)
    return result

stepCPU :: CpuState -> IO CpuState
stepCPU cpu = do
    instrWord <- stepPC
    return cpu

main :: IO ()
main = putStrLn "Hello World!"
