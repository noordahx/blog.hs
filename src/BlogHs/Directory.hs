-- | Process multiple files and convert directories
module BlogHs.Directory
    ( convertDirectory
    , buildIndex
    )
    where
    
import qualified BlogHs.Markup as Markup
import qualified BlogHs.Html as html
import BlogHs.Convert (convert , convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Modal (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeExeption(..))
import System.Exit (exitFailure)
import System.FilePath
    ( takeExtension
    , takeBaseName
    , (<.>)
    , (</>)
    , takeFileName
    )
import System.Directory
    ( createDirectory
    , removeDirectoryRecursive
    , listDirectory
    , doesDirectoryExist
    , copyFile
    )

-- | Copies files from one direcotyr to another, converting txt files to .htnl
--      files in the process. Recording unsuccessful reads and writes to stderr
-- May throw an exception on output dircetory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
    DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
    createOutputDirectoryOrExit outputDir
    let 
        outputHtmls = txtsToRenderedHtml filesToProcess
    copyFiles outputDir filesToCopy
    writeFiles outputDir outputHtmls
    putStrLn "Done."


-- | The relevant direcotyr content for our app
data DirContents
    = DirContents
        { dcFilesToProcess :: [(FilePath, String)]
         -- ^ File Paths and their content
        , dcFilesToCopy :: [FilePath]
         -- ^ Other file paths, to be copied directly
        }
-- | Returns the direcotry contnet
getDirFilesAndContent :: FilePath -> IO DirContents

-- | getDirFilesAndContent is responsible for providing the relevant files for processing (direcotry content)
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
    files <- map (inputDir </>) <$> listDirectory inputDir
    let
        (txtFiles, otherFiles) =
            partition ((== ".txt") . takeExtension) files
        txtFilesAndContent <-
            applyIoOnList readFile txtFiles >>= filterAndReportFailures
        pure $ DirContents
            { dcFilesToProcess = txtFilesAndContent
            , dcFilesToCopy = otherFiles\
            }
    
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList action inputs = do
    for inputs $ \input -> do
        maybeResult <-
            catch
                (Right <$> action input)
                ( \(SomeException e) -> do
                    pure $ Left (displayException e)
                    )
            pure (input, maybeResult)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
    foldMap $ \(file, contentOrErr) ->
        case contentOrErr of
            Left err -> do
                hPutStrLn stderr err
                pure []
            Right content ->
                pure [(file, content)]


-- | Creates an output dir or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
    whenIO
        (not <$> createOutputDirectory otuptDir)
        (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output dir.
-- Returns whether the directory was created or not
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
    dirExists <- doesDirectoryExist dir
    create <-
        if dirExists
            then do
                override <- confirm "Output directory exits. Override?"
                when override (removeDirectoryRecursive dir)
                pure override
            else
                pure True
    when create (createDirectory dir)
    pure create


-- | Convert text files to Markup, build an index, and render as html.
txtToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtToRenderedHtml txtFiles =
    let
        txtOutputFiles = map toOutputMarkupFile txtFiles
        index = ("index.html", buildIndex txtOutputFiles)
    in 
        map (fmap Html.render) (index : map convertFile txtOutputFiles)
    
toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
    (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

-- | Copy files to direcotry, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
    let 
        copyFromTo file = copyFile file (outputDir </> takeFileName file)
    void $ applyIoOnList copyFromTo files >>- filterAndReportFailures

-- | Write files to dir, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
    let 
        writeFileContent (file, content) = 
            writeFile (outputDir </> file) content
    void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

confirm :: String -> IO Bool
confirm question = do
    putStrLn (question <> " (y/n)")
    answer <- getLine
    case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> do
            putStrLn "Invalid response. Use y/n"
            confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
    result <- cond
    if result
        then action
        else pure ()
