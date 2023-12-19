import Html
main :: IO ()
main = 
    putStrLn (render myhtml)

myhtml :: Html
myhtml = 
    html_
        "My title"
        (
            (h1_ "Heading")
            <>
            ( 
                (p_ "Paragraph #1") <>
                (code_ "#define pb push_back")
            )
        )



