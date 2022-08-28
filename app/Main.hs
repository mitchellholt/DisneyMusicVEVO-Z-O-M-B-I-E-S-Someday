{-# LANGUAGE BlockArguments #-}
module Main where

import qualified ParseFile
import qualified ParseLib
import qualified ParseProof

import Verify

import System.IO
import GHC.IO.Handle (hIsEOF)


verifyTheorem :: ParseFile.Theorem -> Bool
verifyTheorem (_, s, p) = (s == (top p)) && verify p


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
    fHandle <- openFile "test1.jk" ReadMode
    contents <- readWholeFile fHandle
    hClose fHandle
    case ParseLib.parse ParseFile.language contents of
        Nothing -> do
            print "sadj"
        Just (l, _) -> do
            case (head l) of
                Left (_, _, p) -> do
                    print p
                    putStrLn $ contents ++ "\n"
                    putStrLn $ if (verify p) then "SO TRUEEEE!!!!" else "\nnot so true :( "
                    return ()
                Right b -> do
                    print b

    -- case ParseLib.parse ParseProof.proof contents of
    --     Nothing -> do
    --         print "Could not parse file"
    --         return ()
    --     Just (l, _) -> do
    --         putStrLn $ contents ++ "\n"
    --         putStrLn $ if (verify l) then "SO TRUEEEE!!!!" else "\n:("
    --         return ()
