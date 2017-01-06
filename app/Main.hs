module Main where

import qualified Dcpu16 as D
import System.FilePath (takeExtension)
import Options.Applicative
import Data.Char

data Opts = Opts { prog :: String }

opts :: Parser Opts
opts = Opts <$> argument str (metavar "PROG" <> help "assembler or binary program" )

main :: IO ()
main = do 
    (Opts prog) <- execParser fullOpts
    emu <- D.newEmulator
    let isAsm = map toLower (takeExtension prog) `elem` [".asm", ".dasm16"]
    if isAsm
        then do putStrLn $ "Loading asm program " ++ prog
                D.loadAsmProgram emu prog
        else do putStrLn $ "Loading binary program " ++ prog
                D.loadBinaryProgram emu prog
    D.runEmulatorLoop emu
  where
    fullOpts = info (helper <*> opts) (fullDesc <> progDesc "DCPU-16 Emulator")