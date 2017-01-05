{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Dcpu16.Cpu where

import Dcpu16.Utils
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word
import Data.Bits
import Control.Monad
import Control.Applicative ((<$>))

screenWidth = 128 :: Int
screenHeight = 96 :: Int 
screenScale = 4   :: Int
gfxStart = 0x8000 :: Int
spritesStart = 0x9050 :: Int
spriteCount = 16 :: Int
inputStart = 0x9000 :: Int
inputMaxCount = 0x10 :: Int

memorySize = 0x10000

data Reg = RegA
         | RegB
         | RegC
         | RegX
         | RegY
         | RegZ
         | RegI
         | RegJ
         | RegPC  -- program counter
         | RegSP  -- stack pointer
         | RegEx
         deriving (Show, Bounded, Enum)

data CpuState = CpuState { memory :: MV.IOVector Word16
                         , regs :: MV.IOVector Word16
                         }

data Instr = Set
           | Add
           | Sub
           | Mul
           | Div
           | Mod
           | Shl
           | Shr
           | And
           | Bor
           | Xor
           | Ife
           | Ifn
           | Ifg
           | Ifb
           | Jsr
           | Dat
           deriving (Show, Enum)

data Value = ValueReg Reg           -- register
           | ValueAddrReg Reg       -- [regiter]
           | ValueAddrRegPlus Reg Word16    -- [next word + register]
           | ValuePop
           | ValuePeek
           | ValuePush
           | ValueSP
           | ValuePC
           | ValueO
           | ValueAddr Word16        -- [next word]
           | ValueLit Word16
           | ValueSymLit Word16
           deriving (Show)

type InstrItem = (Instr, Value, Value)   

regCount = fromEnum (maxBound :: Reg) + 1

newCpu :: IO CpuState
newCpu = do
    mem <- MV.new memorySize
    regs <- MV.new regCount
    return CpuState {memory = mem, regs = regs }

readMemory :: CpuState -> Int -> IO Word16
readMemory CpuState {memory = memory} = MV.read memory

writeMemory :: CpuState -> Int -> Word16 -> IO ()
writeMemory CpuState {memory = memory} = MV.write memory

readRegister :: CpuState -> Reg -> IO Word16
readRegister CpuState {regs = regs} reg = MV.read regs (fromEnum reg)

writeRegister :: CpuState -> Reg -> Word16 -> IO ()
writeRegister CpuState {regs = regs} reg = MV.write regs (fromEnum reg)

incPC :: CpuState -> IO Word16
incPC cpu = do
    addr <- readRegister cpu RegPC
    result <- readMemory cpu (fromIntegral addr)
    writeRegister cpu RegPC (addr + 1)
    return result


parseInstrParts :: Word16 -> (Word16, Word16, Word16)
parseInstrParts w = if oo == 0 then (aa + 0xf, bb, 0) else (oo, aa, bb)
-- a basic instruction format:    bbbbbbaaaaaaoooo
-- a non-basic insruction format: aaaaaaoooooo0000
    where (oo, aa, bb) = (w .&. 0xf, (w `shiftR` 4) .&. 0x3f, (w `shiftR` 10) .&. 0x3f)

withNextWord :: CpuState -> (Word16 -> a) -> IO a
withNextWord cpu f = do
    w <- incPC cpu
    return $ f w

readValue :: CpuState -> Word16 -> IO Value
readValue cpu w = do
    let reg w = toEnum $ fromIntegral w :: Reg
    case w of
        w | w <= 0x07 -> return $ ValueReg $ reg w
        w | w <= 0x0f -> return $ ValueAddrReg $ reg $ w - 0x08
        w | w <= 0x17 -> withNextWord cpu $ ValueAddrRegPlus (reg $ w - 0x10)
        0x18 -> return ValuePop
        0x19 -> return ValuePeek
        0x1a -> return ValuePush
        0x1b -> return ValueSP
        0x1c -> return ValuePC
        0x1d -> return ValueO
        0x1e -> withNextWord cpu ValueAddr
        0x1f -> withNextWord cpu ValueLit
        w | w <= 0x3f -> return $ ValueLit (w - 0x20)
        _ -> fail "readValue: wrong value"

readInstr :: CpuState -> IO InstrItem
readInstr cpu = do
    w <- incPC cpu
    let (oo, aa, bb) = parseInstrParts w
    let instr = toEnum (fromIntegral $ oo - 1) :: Instr
    a <- readValue cpu aa
    b <- readValue cpu bb
    return (instr, a, b)

getValue :: CpuState -> Value -> IO Word16
getValue cpu (ValueReg reg) = readRegister cpu reg
getValue cpu (ValueAddrReg reg) = do
    w <- readRegister cpu reg
    readMemory cpu (fromIntegral w)
getValue cpu (ValueAddrRegPlus reg next) = do
    w <- readRegister cpu reg
    let addr = w + next
    readMemory cpu (fromIntegral addr)
getValue cpu ValuePop = do
    sp <- readRegister cpu RegSP
    writeRegister cpu RegSP (sp + 1)
    readMemory cpu (fromIntegral sp)
getValue cpu ValuePeek = do 
    sp <- readRegister cpu RegSP
    readMemory cpu (fromIntegral sp)
getValue cpu ValuePush = do
    sp0 <- readRegister cpu RegSP
    let sp = sp0 - 1
    writeRegister cpu RegSP sp
    readMemory cpu (fromIntegral sp)
getValue cpu ValueSP = readRegister cpu RegSP
getValue cpu ValuePC = readRegister cpu RegPC
getValue cpu ValueO = readRegister cpu RegEx
getValue cpu (ValueAddr addr) = readMemory cpu (fromIntegral addr)
getValue cpu (ValueLit lit) = return lit
getValue cpu (ValueSymLit lit) = return lit

setValue :: CpuState -> Value -> Word16 -> IO ()
setValue cpu (ValueReg reg) v = writeRegister cpu reg v
setValue cpu (ValueAddrReg reg) v = do
    w <- readRegister cpu reg
    writeMemory cpu (fromIntegral w) v
setValue cpu (ValueAddrRegPlus reg next) v = do
    w <- readRegister cpu reg
    let addr = w + next
    writeMemory cpu (fromIntegral addr) v
setValue cpu ValuePop v = do
    sp <- readRegister cpu RegSP
    writeRegister cpu RegSP (sp + 1)
    writeMemory cpu (fromIntegral sp) v
setValue cpu ValuePeek v = do 
    sp <- readRegister cpu RegSP
    writeMemory cpu (fromIntegral sp) v
setValue cpu ValuePush v = do
    sp0 <- readRegister cpu RegSP
    let sp = sp0 - 1
    writeRegister cpu RegSP sp
    writeMemory cpu (fromIntegral sp) v
setValue cpu ValueSP v = writeRegister cpu RegSP v
setValue cpu ValuePC v = writeRegister cpu RegPC v
setValue cpu ValueO  v = writeRegister cpu RegEx  v
setValue cpu (ValueAddr addr) v = writeMemory cpu (fromIntegral addr) v
setValue cpu (ValueLit lit) v = return () -- TODO: warn
setValue cpu (ValueSymLit lit) v = return () -- TODO: warn

evalInstr :: CpuState -> InstrItem -> IO ()
evalInstr cpu (Set, dst, src) = do
    w <- getValue cpu src
    setValue cpu dst w
evalInstr cpu (Add, a, b) = evalArithInstr cpu a b op
    where op a b = let res = a + b in (res, if res > 0xffff then 1 else 0)
evalInstr cpu (Sub, a, b) = evalArithInstr cpu a b op
    where op a b = let res = a - b in (res, if res < 0 then 0xffff else 0)
evalInstr cpu (Mul, a, b) = evalArithInstr cpu a b op
    where op a b = let res = a * b in (res, (res `shiftR` 16) .&. 0xffff)
evalInstr cpu (Div, a, b) = evalArithInstr cpu a b op
    where op a b = if b == 0 then (0, 0) else (a`div`b, ((a `shiftL` 16) `div` b) .&. 0xffff)
evalInstr cpu (Mod, a, b) = evalArithInstr cpu a b op
    where op a b = if b == 0 then (0, -1) else (a`mod`b, -1)
evalInstr cpu (Shl, a, b) = evalArithInstr cpu a b op
    where op a b = let res = a `shiftL` b in (res, (res `shiftR` 16) .&. 0xffff)
evalInstr cpu (Shr, a, b) = evalArithInstr cpu a b op
    where op a b = let res = a `shiftR` b in (res, ((res `shiftL` 16) `shiftR` b).&. 0xffff)
evalInstr cpu (And, a, b) = evalArithInstr cpu a b op
    where op a b = if b == 0 then (0, -1) else (a .&. b, -1)
evalInstr cpu (Bor, a, b) = evalArithInstr cpu a b op
    where op a b = if b == 0 then (0, -1) else (a .|. b, -1)
evalInstr cpu (Xor, a, b) = evalArithInstr cpu a b op
    where op a b = if b == 0 then (0, -1) else (a `xor` b, -1)
evalInstr cpu (Ife, a, b) = evalIfInstr cpu a b (==)
evalInstr cpu (Ifn, a, b) = evalIfInstr cpu a b (/=)
evalInstr cpu (Ifg, a, b) = evalIfInstr cpu a b (>)
evalInstr cpu (Ifb, a, b) = evalIfInstr cpu a b (\a b -> (a .&. b) /= 0)
evalInstr cpu (Jsr, a, _) = do
    evalInstr cpu (Set, ValuePush, ValuePC)
    evalInstr cpu (Set, ValuePC, a)
evalInstr cpu (Dat, _, _)  = return ()

evalArithInstr :: CpuState -> Value -> Value -> (Int -> Int -> (Int, Int)) -> IO ()
evalArithInstr cpu a b op = do
    aa <- fromIntegral <$> getValue cpu a
    bb <- fromIntegral <$> getValue cpu b
    let (result, ex) = op aa bb
    when (ex >= 0) $ writeRegister cpu RegEx $ fromIntegral ex
    setValue cpu a $ fromIntegral result

evalIfInstr :: CpuState -> Value -> Value -> (Word16 -> Word16 -> Bool) -> IO ()
evalIfInstr cpu a b op = do
    aa <- getValue cpu a
    bb <- getValue cpu b
    unless (op aa bb) $ void $ readInstr cpu

writeMemoryBlock :: CpuState -> [Word16] -> IO ()
writeMemoryBlock cpu arr = 
    forM_ (zip [0..] arr) (\(i, w) -> writeMemory cpu i w)

run :: CpuState -> IO ()
run cpu = do
    instr <- readInstr cpu
    evalInstr cpu instr    
