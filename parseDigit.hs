parseDigit :: Char -> Either ParseDigitError Int
parseDigit c =
    case c of
        '0' -> Right 0
        '1' -> Right 1
        '2' -> Right 2
        '3' -> Right 3
        '4' -> Right 4
        '5' -> Right 5
        '6' -> Right 6
        '7' -> Right 7
        '8' -> Right 8
        '9' -> Right 9
        _ -> Left (NotADigit c)

data ParseDigitError
    = NotADigit Char
    deriving Show

max3chars :: Char -> Char -> Char -> Either ParseDigitError Int
max3chars x y z =
    (\a b c -> max a (max b c))
        <$> parseDigit x
        <*> parseDigit y
        <*> parseDigit z