module Pattern where
    fly :: Int -> String
    fly 30 = "Time flies like an arrow"
    fly 50 = "Fruit files like a banana"
    fly n = show n

    deep :: String -> String
    deep (' ':' ':xs) = "    " ++ xs
    deep (' ':xs) = "  " ++ xs
    deep xs = xs

    deep' :: String -> String
    deep' s@(' ':' ':_) = "    " ++ s
    deep' s@(' ':_) = "  " ++ s
    deep' s = s