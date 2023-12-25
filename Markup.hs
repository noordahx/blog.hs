module Markup
    ( Document
    , Structure(..)
    )
where
    
import Numeric.Natural
import Data.Maybe (maybeToList)

type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving Show

data Context
    = CtxHeading Natural String
    | CtxParagraph [String]
    | CtxUnorderedList [String]
    | CtxOrderedList [String]
    | CtxCodeBlock [String]

-- parse :: String -> Document
-- parse = parseLines [] . lines

-- parseLines :: [String] -> [String] -> Document
-- parseLines currentParagraph txts =
--     let
--         paragraph = Paragraph (unlines (reverse currentParagraph))
--     in
--         case txts of
--             [] -> [paragraph]
--             currentLine : rest ->
--                 if trim currentLine == ""
--                     then
--                         paragraph : parseLines [] rest
--                     else
--                         parseLines (currentLine : currentParagraph) rest

-- trim :: String -> String
-- trim = unwords . words

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts = 
    case txts of
        -- done case or empty
        [] -> maybeToList context

        -- Heading 1 case
        ('*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
        
        -- Unordered list case
        ('-' : ' ' : line) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just (UnorderedList (list <> [trim line]))) rest
                _ ->
                    maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
        
        -- Ordered list case
        ('#' : ' ' : line) : rest ->
            case context of
                Just (OrderedList list) ->
                    parseLines (Just (OrderedList (list <> [trim line]))) rest
                _ ->
                    maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

        -- CodeBlock case
        ('>' : ' ' : line) : rest ->
            case context of
                Just (CodeBlock code) ->
                    parseLines (Just (CodeBlock (code <> [line]))) rest
                _ ->
                    maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

        -- Paragraph case
        currentLine : rest ->
            let 
                line = trim currentLine
            in 
                if line == ""
                    then
                        maybe id (:) context (parseLines Nothing rest) -- same as below
                        -- (case context of
                        --     Nothing -> id
                        --     Just structure -> (:) structure
                        -- ) (parseLines Nothing rest)
                    else 
                        case context of
                            Just (Paragraph paragraph) ->
                                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
                            _ ->
                                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
                        
