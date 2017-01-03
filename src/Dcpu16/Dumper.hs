module Dcpu16.Dumper (dump) where

import Dcpu16.Cpu
import Text.Printf

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