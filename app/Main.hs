{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.Text.Lazy.IO as TIO (readFile, putStrLn)
import qualified Data.Text.Lazy.Encoding as TE (decodeUtf8)

import Telly (tvToJSON)

usage :: IO ()
usage = putStrLn "telly <html file>"

run :: String -> IO ()
run file = do
    htmlData <- TIO.readFile file
    let jsonData = tvToJSON htmlData
    TIO.putStrLn $ TE.decodeUtf8 jsonData

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage >> exitFailure
        (f:_) -> run f

