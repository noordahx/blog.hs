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
        ConvertDir input output ->
            BlogHs.convertDirectory input output
        
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

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    let 
        previews =
            map
            ( \(file, doc) ->
                case doc of
                    Markup.Heading 1 heading : article ->
                        Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                        <> foldMap convertStructure (take 3 article)
                        <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                    _->
                        Html.h_ 3 (Html.link_ file (Html.txt_ file))
            )
            files
        in
            Html.html_
                "Blog"
                ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
                <> Html.h_ 2 (Html.txt_ "Posts")
                <> mconcat previews
            )
