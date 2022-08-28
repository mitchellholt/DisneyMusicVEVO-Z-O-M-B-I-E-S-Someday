module File where

import System.IO
import System.Environment

getFileContents :: IO String
getFileContents = do
    args <- getArgs
    if null args then return "Please supply a file name"
    else do
        fHandle <- openFile (head args) ReadMode
        contents <- readWholeFile fHandle
        hClose fHandle
        return contents


readWholeFile :: Handle -> IO String
readWholeFile fHandle = do
    isEnd <- hIsEOF fHandle
    if isEnd
       then do return ""
    else do
        this <- hGetLine fHandle
        rest <- readWholeFile fHandle
        return (this ++ "\n" ++ rest)