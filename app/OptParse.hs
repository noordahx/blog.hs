module OptParse
  ( Options (..),
    SingleInput (..),
    SingleOutput (..),
    parse,
  )
where

import BlogHs.Env

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath Env
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

-- * Parser

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info
    (pOptions <**> helper)
    ( fullDesc
        <> header "blog-hs-gen - a static blog generator"
        <> progDesc "Convert markup files or directories to html"
    )

-- | Parser for al options
pOptions :: Parser Options
pOptions =
  subparser
    ( command
        "convert"
        ( info
            (helper <*> pConvertSingle)
            (progDesc "Convert a single markup source to html")
        )
        <> command
          "convert-dir"
          ( info
              (helper <*> pConvertDir)
              (progDesc "Convert a directory of markup files to html")
          )
    )

-- * Single source to sink conversiion parser

-- | Parser for single source to sink option
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

-- | Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- | Input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file"
        )

-- | Ou file parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file"
        )

--------------------------------------

-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

-- | Parser for blog environment

pEnv :: Parser Env
pEnv = 
  Env <$> pBlogName <*> pStylesheet


-- | Blog name parser
pBlogName :: Parser String
pBlogName =
  strOption
    ( long "name"
      <> short 'N'
      <> metavar "STRING"
      <> help "Blog name"
      <> value (eBlogName defaultEnv)
      <> showDefault
    )


-- | Stylesheet parser
pStylesheet :: Parser String
pStylesheet =
  strOption
	  ( long "style"
		  <> short 'S'
			<> metavar "FILE"
			<> help "Stylesheet filename"
			<> value (eStylesheetPath defaultEnv)
			<> showDefault
		)

-- | Parser for input directory
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "DIRECTORY"
        <> help "Input directory"
    )

-- | Parser for output directory
pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIRECTORY"
        <> help "Output directory"
    )

