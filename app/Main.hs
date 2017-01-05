module Main where

import Dcpu16
import Dcpu16.Assembler

main :: IO ()
main = runEmulatorLoop "tests/pacman.dasm16"
