module Main where

import OptParse
import qualified BlogHs

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
    options <- parse
    case options of 
        ConvertDir input output env ->
            BlogHs.convertDirectory env input output
        
        ConvertSingle input output -> do
            (title, inputHandle) <-
                case input of
                    Stdin ->
                        pure ("", stdin)
                    InputFile file ->
                        (,) file <$> openFile file ReadMode

            outputHandle <-
                case output of
                    Stdout -> pure stdout
                    OutputFile file -> do
                        exists <- doesFileExist file
                        shouldOpenFile <-
                            if exists
                                then confirm
                                else pure True
                        if shouldOpenFile
                            then
                                openFile file WriteMode
                            else
                                exitFailure
            
            BlogHs.convertSingle title inputHandle outputHandle
            hClose inputHandle
            hClose outputHandle

-- * Utils

confirm :: IO Bool
confirm =
    putStrLn "Are you sure? (y/n)" *>
        getLine >>= \answer ->
            case answer of
                "y" -> pure True
                "n" -> pure False
                _ -> putStrLn "Invalid response. use y or n" *>
                    confirm

