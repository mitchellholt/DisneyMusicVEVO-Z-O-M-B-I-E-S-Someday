{-# LANGUAGE BlockArguments #-}
module Main where

import ParseFile
import ParseLib
import ParseProof
import Verify
import File

import System.IO
import GHC.IO.Handle (hIsEOF)


verifyTheorem :: ParseFile.Section -> Bool
verifyTheorem (Left (_, s, p)) = (s == top p) && verify p
verifyTheorem _         = True


readWholeFile :: Handle -> IO String
readWholeFile fHandle = do
    isEnd <- hIsEOF fHandle
    if isEnd
       then do return ""
    else do
        this <- hGetLine fHandle
        rest <- readWholeFile fHandle
        return (this ++ "\n" ++ rest)

main :: IO ()
main = do
    contents <- File.gerFileContents
    case ParseLib.parse ParseFile.language contents of
        Nothing -> do
            print "sadj"
        Just (l, _) -> if all (verifyTheorem <$> l)
            then do print "sotrue"
            else do print ":("
