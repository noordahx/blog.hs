<<<<<<< HEAD
import Html
main :: IO ()
main = 
    putStrLn (render myhtml)

myhtml :: Html
myhtml = 
    html_
        "My title"
        ( append_
            (h1_ "Heading")
            ( append_
                (p_ "Paragraph #1")
                (code_ "#define pb push_back")
            )
        )



=======
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



>>>>>>> 9330fac98f0fba8dba2217c64ee03da08055802d
