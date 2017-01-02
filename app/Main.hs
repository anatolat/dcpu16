{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV 
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import Data.Word
import Data.Bits
import Control.Monad
import Control.Applicative ((<$>))
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import qualified Foreign.Storable as FS
import qualified Foreign.ForeignPtr as FP

import qualified SDL
import Control.Concurrent (threadDelay)

import Data.IORef -- temp

screenWidth = 128 :: Int
screenHeight = 96 :: Int 
screenScale = 4   :: Int
gfxStart = 0x8000

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

regCount = fromEnum (maxBound :: Reg) + 1

data CpuState = CpuState { memory :: MV.IOVector Word16
                          , regs :: MV.IOVector Word16
                          }


newState :: IO CpuState
newState = do
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

stepPC :: CpuState -> IO Word16
stepPC cpu = do
    addr <- readRegister cpu RegPC
    result <- readMemory cpu (fromIntegral addr)
    writeRegister cpu RegPC (addr + 1)
    return result

stepCPU :: CpuState -> IO CpuState
stepCPU = undefined
--stepCPU cpu = do
--    instrWord <- stepPC
--    return cpu


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
           deriving (Show, Bounded, Enum)

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
           deriving (Show)

type InstrItem = (Instr, Value, Value)

parseInstrParts :: Word16 -> (Word16, Word16, Word16)
parseInstrParts w = if oo == 0 then (aa + 0xf, bb, 0) else (oo, aa, bb)
-- a basic instruction format:    bbbbbbaaaaaaoooo
-- a non-basic insruction format: aaaaaaoooooo0000
    where (oo, aa, bb) = (w .&. 0xf, (w `shiftR` 4) .&. 0x3f, (w `shiftR` 10) .&. 0x3f)

withNextWord :: CpuState -> (Word16 -> a) -> IO a
withNextWord cpu f = do
    w <- stepPC cpu
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
    w <- stepPC cpu
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

testProg :: [Word16]
testProg = [
    0x7c01, 0x0030, 0x7de1, 0x1000, 0x0020, 0x7803, 0x1000, 0xc00d,
    0x7dc1, 0x001a, 0xa861, 0x7c01, 0x2000, 0x2161, 0x2000, 0x8463,
    0x806d, 0x7dc1, 0x000d, 0x9031, 0x7c10, 0x0018, 0x7dc1, 0x001a,
    0x9037, 0x61c1, 0x7dc1, 0x001a, 0x0000, 0x0000, 0x0000, 0x0000]

writeMemoryBlock :: CpuState -> [Word16] -> IO ()
writeMemoryBlock cpu arr = 
    forM_ (zip [0..] arr) (\(i, w) -> writeMemory cpu i w)

loadBinProg :: CpuState -> FilePath -> IO ()
loadBinProg cpu path = do  
    bs <- BS.readFile path
    writeMemoryBlock cpu $ bytesToWords $ BS.unpack bs

start :: IO CpuState
start = do
    cpu <- newState
    loadBinProg cpu "pacman-1.1.bin"
    return cpu

run :: CpuState -> IO ()
run cpu = do
    instr <- readInstr cpu
    evalInstr cpu instr    

dumpMemLine :: CpuState -> Int -> IO ()
dumpMemLine cpu addr = do
    putStr (printf "%04X:" addr)
    forM_ [0..7] (\i -> readMemory cpu (addr + i) >>= putStr . printf " %04X")
    putStrLn ""

dumpMemPage :: CpuState -> Int -> IO ()
dumpMemPage cpu base = forM_ [0,8..56] (\i -> dumpMemLine cpu (base + i))

dump :: CpuState -> IO ()
dump cpu = do
    pc <- readRegister cpu RegPC
    sp <- readRegister cpu RegSP
    a <- readRegister cpu RegA
    b <- readRegister cpu RegB
    c <- readRegister cpu RegC
    x <- readRegister cpu RegX
    y <- readRegister cpu RegY
    z <- readRegister cpu RegZ
    i <- readRegister cpu RegI
    j <- readRegister cpu RegJ
    putStrLn $ printf "PC: %04X  SP: %04X" pc sp
    putStrLn $ printf "A:  %04X  B:  %04X C:  %04X" a b c
    putStrLn $ printf "X:  %04X  Y:  %04X Z:  %04X" x y z
    putStrLn $ printf "I:  %04X  J:  %04X" i j
    putStrLn "\nMemory dump:"
    dumpMemPage cpu 0
    putStrLn ""

pallete :: SV.Vector Word32
pallete = SV.fromList 
            [ 0x000000FF, 0x1B2632FF, 0x493C2BFF, 0x2F484EFF
            , 0x005784FF, 0xBE2633FF, 0x44891AFF, 0xA46422FF
            , 0x31A2F2FF, 0xE06F8BFF, 0xEB8931FF, 0x9D9D9DFF
            , 0xA3CE27FF, 0xB2DCEFFF, 0xFFE26BFF, 0xFFFFFFFF]

writeTexturePixel :: (MV.IOVector Word32) -> Int -> Int -> Word16 -> IO ()
writeTexturePixel screen x y pixel = do
    let color = pallete SV.! (fromIntegral pixel)
    MV.write screen (y * screenWidth + x) color

drawBg :: CpuState -> (MV.IOVector Word32) -> IO ()
drawBg cpu screen = do
    let yx = [(y, x) | y <- [0..screenHeight-1], x <- [0..screenWidth-1]]
    forM_ yx (\(y, x) -> do
        let addr = gfxStart + x `div` 3 + (y `div` 3) * (screenWidth `div` 3)
        w <- readMemory cpu addr
        let c1 = w `shiftR` 12
        let c2 = (w `shiftR` 8) .&. 0xf
        let idx = x `mod` 3 + (y `mod` 3) * 3
        let b = w .&. (0x80 `shiftR` idx)
        writeTexturePixel screen x y (if b /= 0 then c1 else c2)
        return ())

updateScreen :: CpuState -> (MV.IOVector Word32) -> IO ()
updateScreen = drawBg

sizeOfElem :: forall a m. (FS.Storable a) => (MV.MVector m a) -> Int
sizeOfElem vec = FS.sizeOf (undefined :: a)

mvectorToByteString :: (FS.Storable a) => MV.IOVector a -> BS.ByteString
mvectorToByteString vec
  = BS.fromForeignPtr (FP.castForeignPtr fptr) (scale off) (scale len)  where
    (fptr, off, len) = MV.unsafeToForeignPtr vec
    scale = (* sizeOfElem vec)
    
mainLoop :: IO ()
mainLoop = do
    SDL.initialize [SDL.InitVideo]

    let windowSize = SDL.V2 (fromIntegral screenWidth) (fromIntegral screenHeight)
    let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
    window <- SDL.createWindow "DCPU-16" windowConfig
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming windowSize

    cpu <- start
    screenBuf <- MV.new (screenWidth * screenHeight)
    counter <- newIORef 0 :: IO (IORef Int)

    let loop = do
            events <- SDL.pollEvents
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            modifyIORef counter (+1)
            counterValue <- readIORef counter
            putStrLn $ "Step " ++ show counterValue
            run cpu
            updateScreen cpu screenBuf

            let screenBs = mvectorToByteString screenBuf
            SDL.updateTexture texture Nothing screenBs (fromIntegral $ screenWidth * 4)

            SDL.copy renderer texture Nothing Nothing
            SDL.present renderer

            unless quit loop
    loop

    SDL.destroyWindow window
    SDL.quit

toWord16 :: Word8 -> Word8 -> Word16
toWord16 lsb msb = (fromIntegral lsb .|. ((fromIntegral msb) `shiftL` 8))

bytesToWords :: [Word8] -> [Word16]
bytesToWords = go
    where go [] = []
          go (x:y:xs) = toWord16 x y : go xs
          

main :: IO ()
main = mainLoop
