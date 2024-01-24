module BlogHs.Env where
data Env
    = Env
        { eBlogName :: !String
        , eStylesheetPath :: !FilePath 
        }
    deriving Show

defaultEnv :: Env
defaultEnv = Env "MyBlog" "style.css"

