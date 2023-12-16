main :: IO ()
main = 
    putStrLn myhtml

myhtml :: String
myhtml = makeHtml 
    "Hello Title" 
    (h1_ "Hello, World!" <> p_ "Let's learn about Haskell!!")


makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

newtype Html = Html String
newtype Structure = Structure String

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_  = el "head"

title_ :: String -> String
title_ = el "title"

-- p_ :: String -> String
-- p_ = el "p"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> String
h1_ = el "h1"

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"



