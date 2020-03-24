module Main where

import System.IO
import System.Environment
import System.Exit

import Assembler

main :: IO ()
main = do
    (inputFile, outputFile) <- (getArgs >>= parse)
    readHandle <- openFile inputFile ReadMode
    programText <- hGetContents readHandle

    assembledLines <- case (assemble programText) of
                        Left err -> (die (show err) >> (pure []))
                        Right lines -> pure lines

    writeHandle <- openFile outputFile WriteMode
    mapM (hPutStrLn writeHandle) assembledLines
    hFlush writeHandle
    hClose writeHandle
    hClose readHandle
    exitSuccess

parse :: [String] -> IO (String, String)
parse [f] = pure (f, "output.obj")
parse [f1, f2] = pure (f1, f2)
parse _ = usage

usage :: IO a
usage = die "usage: assembler input_file [output_file]"
