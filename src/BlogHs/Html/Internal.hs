module BlogHs.Html.Internal where

import Numeric.Natural

-- * Types

newtype Html
    = Html String

newtype Structure
    = Structure String

type Title
    = String

-- * Embedded Domain Specific Language HTML

html_ :: Title -> Structure -> Html
html_ title content =
    Html
    ( el "html"
        ( el "head" (el "title" (escape title))
          <> el "body" (getStructureString content) 
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ n  = Structure . el ("h" <> show n) . escape

instance Monoid Structure where
    mempty = Structure ""
    -- mempty = empty_

instance Semigroup Structure where
    -- (<>) :: Structure -> Structure -> Structure
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)
        
-- append_ :: Structure -> Structure -> Structure
-- append_ (Structure a) (Structure b) =
--     Structure (a <> b)
-- append_ c1 c2 = 
--     Structure (getStructureString c1 <> getStructureString c2)

li_ :: Structure -> String
li_ =
    el "li" . getStructureString

ul_ :: [Structure] -> Structure
ul_ = 
    Structure . el "ul" . concat . map li_

ol_ :: [Structure] -> Structure
ol_ =
    Structure . el "ol" . concat . map li_

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- * Render

render :: Html -> String
render html =
    case html of
        Html str -> str


-- * Utils

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
    case content of
        Structure str -> str

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
    in
        concat . map escapeChar

-- empty_ :: Structure
-- empty_ = Structure ""

-- concatStructure :: [Structure] -> Structure
-- concatStructure list =
--     case list of 
--         [] -> empty_
--         x : xs -> x <> concatStructure xs

mconcat :: Monoid a => [a] -> a
mconcat list =
    case list of
        [] -> mempty
        x : xs -> x <> BlogHs.Html.Internal.mconcat xs