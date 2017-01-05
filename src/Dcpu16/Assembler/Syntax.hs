module Dcpu16.Assembler.Syntax where

import Data.Word
import Dcpu16.Cpu

data AValue = AValue Value
            | AValueSym String      -- symbol
            | AValueSymAddr String  -- [symbol]
            | AValueSymAddrPlusLit String Word16 -- [symbol + lit]
            | AValueSymAddrPlusReg String Reg -- [symbol + reg]
            deriving (Show)

data AInstr = AInstr Instr AValue AValue
            | AInstrDat [Word16]
            | AInstrLabel String
            deriving (Show)


valueSize :: Value -> Int
valueSize (ValueAddrRegPlus _ _) = 1
valueSize (ValueAddr _ ) = 1
valueSize (ValueLit w) = if w <= 0x1f then 0 else 1
valueSize (ValueSymLit 0) = 0
valueSize _ = 0

asmValueSize :: AValue -> Int
asmValueSize (AValue value) = valueSize value
asmValueSize _ = 1

asmInstrSize :: AInstr -> Int
asmInstrSize (AInstr Jsr a _) = 1 + asmValueSize a
asmInstrSize (AInstr _ a b) = 1 + asmValueSize a + asmValueSize b
asmInstrSize (AInstrDat ws) = length ws
asmInstrSize (AInstrLabel _) = 0