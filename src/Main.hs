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

main :: IO ()
main = do
    contents <- File.getFileContents
    case ParseLib.parse ParseFile.language contents of
        Just (l, "") -> if (and (verifyTheorem <$> l))
            then do putStrLn "sotrue"
            else do putStrLn ":("
        _ -> do
            putStrLn "sadj"
