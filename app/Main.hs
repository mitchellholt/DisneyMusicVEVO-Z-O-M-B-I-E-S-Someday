{-# LANGUAGE BlockArguments #-}
module Main where

import qualified ParseLib (parse, token)
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
    fHandle <- openFile "test.txt" ReadMode
    contents <- readWholeFile fHandle
    hClose fHandle
    case ParseLib.parse (ParseLib.token "thm.") contents of
      Just (str, rest) -> putStrLn str
      Nothing       -> putStrLn "err"
