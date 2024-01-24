module BlogHs
    ( process
    , convertSingle
    , convertDirectory
    ) 
    where

import BlogHs.Env (defaultEnv)
import BlogHs.Directory (convertDirectory, buildIndex)
import qualified BlogHs.Markup as Markup
import qualified BlogHs.Html as Html
import BlogHs.Convert (convert)

import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
    content <- hGetContents input
    hPutStrLn output (process title content)


process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
