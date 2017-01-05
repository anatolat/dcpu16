module Main where

import Dcpu16

main :: IO ()
main = do
    emu <- newEmulator
    loadAsmProgram emu "tests/pacman.dasm16"
    runEmulatorLoop emu
