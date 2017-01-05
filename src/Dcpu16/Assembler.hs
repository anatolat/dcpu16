module Dcpu16.Assembler 
    ( module Dcpu16.Assembler.Syntax
    , module Dcpu16.Assembler.Parser
    , compileFile
    , compileFileToVec
    , compileInstructions
    ) where

import Dcpu16.Cpu
import Dcpu16.Utils
import Dcpu16.Assembler.Syntax
import Dcpu16.Assembler.Parser
import Data.Word
import Data.List (foldl')
import Control.Monad (foldM_)
import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as SV

buildLabelMap :: [AInstr] -> (Map.Map String Int, Int)
buildLabelMap instrs = foldl' go (Map.empty, 0) instrs
    where 
        go (mp, offs) (AInstrLabel sym) = (Map.insert sym offs mp, offs)
        go (mp, offs) instr = (mp, offs + asmInstrSize instr)

resolveAsmValue :: AValue -> Map.Map String Int -> Value
resolveAsmValue (AValue value) _ = value
resolveAsmValue (AValueSym value) labelMap = 
    ValueSymLit $ fromIntegral $ labelMap Map.! value
resolveAsmValue (AValueSymAddr value) labelMap = 
    ValueAddr $ fromIntegral $ labelMap Map.! value
resolveAsmValue (AValueSymAddrPlusLit label w) labelMap = 
    ValueSymLit $ (fromIntegral $ labelMap Map.! label) + w
resolveAsmValue (AValueSymAddrPlusReg label reg) labelMap =
    ValueAddrRegPlus reg $ fromIntegral $ labelMap Map.! label
    
resolveAsmInstruction :: AInstr -> Map.Map String Int -> [InstrItem]
resolveAsmInstruction (AInstr instr a b) labelMap = [(instr, resolveAsmValue a labelMap, resolveAsmValue b labelMap)]
resolveAsmInstruction (AInstrDat ws) _ = map (\w -> (Dat, ValueLit w, ValueLit 0)) ws 
resolveAsmInstruction (AInstrLabel _) _ = []

resolveAsmInstructions :: [AInstr] -> Map.Map String Int -> [InstrItem]
resolveAsmInstructions asmInstrs labelMap = concatMap (\i -> resolveAsmInstruction i labelMap) asmInstrs

compileValue :: Value -> MV.IOVector Word16 -> Int -> IO (Word16, Int)
compileValue (ValueReg reg) _ ptr = return (wreg, ptr)
    where wreg = fromIntegral $ fromEnum reg
compileValue (ValueAddrReg reg) _ ptr = return (wreg + 0x08, ptr)
    where wreg = fromIntegral $ fromEnum reg
compileValue (ValueAddrRegPlus reg nw) buf ptr = do
    let wreg = fromIntegral $ fromEnum reg 
    MV.write buf ptr nw
    return (wreg + 0x10, ptr + 1) 
compileValue ValuePop _ ptr = return (0x18, ptr)
compileValue ValuePeek _ ptr = return (0x19, ptr)
compileValue ValuePush _ ptr = return (0x1a, ptr)
compileValue ValueSP _ ptr = return (0x1b, ptr)
compileValue ValuePC _ ptr = return (0x1c, ptr)
compileValue ValueO _ ptr = return (0x1d, ptr)
compileValue (ValueAddr nw) buf ptr = do
    MV.write buf ptr nw
    return (0x1e, ptr + 1) 
compileValue (ValueLit nw) buf ptr = 
    if nw <= 0x1f 
        then return (0x20 + nw, ptr)
        else MV.write buf ptr nw >> return (0x1f, ptr + 1)
compileValue (ValueSymLit nw) buf ptr = do
    MV.write buf ptr nw
    return (0x1f, ptr + 1)

compileInstr :: InstrItem -> MV.IOVector Word16 -> Int -> IO Int
compileInstr (Dat, a@(ValueLit w), _) buf ptr = do
    putStrLn $ "compile Dat " ++ show a
    MV.write buf ptr w
    return $ ptr + 1
-- a non-basic insruction format: aaaaaaoooooo0000
compileInstr (Jsr, a, _) buf ptr = do
    putStrLn $ "compile Jsr " ++ show a
    (wa, ptr') <- compileValue a buf $ ptr + 1
    let wop = fromIntegral $ fromEnum 1
    let w = (wa `shiftL` 10) .|. (wop `shiftL` 4)
    MV.write buf ptr w
    return $ ptr'
-- a basic instruction format:    bbbbbbaaaaaaoooo    
compileInstr (instr, a, b) buf ptr = do
    putStrLn $ "compile " ++ show instr ++ " " ++ show a ++ ", " ++ show b
    (wa, ptr') <- compileValue a buf $ ptr + 1
    (wb, ptr'') <- compileValue b buf $ ptr'
    let wop = fromIntegral $ fromEnum instr + 1
    let w = wop .|. (wa `shiftL` 4) .|. (wb `shiftL` 10)
    MV.write buf ptr w
    return $ ptr''

compileInstructions :: [AInstr] -> IO (SV.Vector Word16)
compileInstructions asmInstrs = do
    let (labelMap, size) = buildLabelMap asmInstrs
    let instrs = resolveAsmInstructions asmInstrs labelMap
    buf <- MV.new size
    foldM_ (\ptr instr -> compileInstr instr buf ptr) 0 instrs
    SV.unsafeFreeze buf

compileFileToVec :: FilePath -> IO (SV.Vector Word16)
compileFileToVec src = parseFile src >>= compileInstructions

compileFile :: FilePath -> FilePath -> IO ()
compileFile src dst = do
    vec <- compileFileToVec src
    writeVectorToFile vec dst
