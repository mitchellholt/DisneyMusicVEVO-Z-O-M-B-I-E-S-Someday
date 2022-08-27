{-# LANGUAGE BlockArguments #-}
module Main where

import qualified ParseLib (parse, token)
import ParseProof
import Verify
import System.IO
import GHC.IO.Handle (hIsEOF)

readWholeFile :: Handle -> IO String
readWholeFile fHandle = do
    isEnd <- hIsEOF fHandle
    if isEnd
       then do return ""
    else do
        this <- hGetLine fHandle
        rest <- readWholeFile fHandle
        return (this ++ rest)

main :: IO ()
main = do
    fHandle <- openFile "test1.jk" ReadMode
    contents <- readWholeFile fHandle
    hClose fHandle
    case ParseLib.parse (proof) contents of
      Just (a, rest) -> putStrLn $ (show a) ++ "\n" ++ (show $ verify a)
      Nothing       -> putStrLn "err :("
