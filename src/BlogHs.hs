module BlogHs
    ( process
    , convertSingle
    , convertDirectory
    ) 
    where

import qualified BlogHs.Markup as Markup
import qualified BlogHs.Html as Html
import BlogHs.Convert (convert)

import System.IO

process :: Html.Title -> String -> String
process title = Html.render .convert title . Markup.parse

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
    content <- hGetContents input
    hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"
