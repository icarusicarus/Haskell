module Guard where
    safeSqrt :: (Ord a, Floating a) => a -> Maybe a
    safeSqrt x
        | x < 0 = Nothing
        | x >= 0 = Just (sqrt x)

    -- combine guard and pattern matching
    caseOfFirstLetter :: String -> String
    caseOfFirstLetter "" = "empty"
    caseOfFirstLetter (x:xs)
        | 'a' <= x && x <= 'z' = "lower"
        | 'A' <= x && x <= 'Z' = "upper"
        | otherwise = "other"

    -- combine case and if
    caseOfFirstLetter' :: String -> String
    caseOfFirstLetter' str =
        case str of
            "" -> ""
            (x:xs) -> if 'a' <= x && x <= 'z'
                        then "lower"
                        else if 'A' <= x && x <= 'Z'
                            then "upper"
                            else "other"

    -- combine guard and case
    caseOfFirstLetter'' :: String -> String
    caseOfFirstLetter'' str =
        case str of
            "" -> ""
            (x:xs)  | 'a' <= x && x <= 'z' -> "lower"
                    | 'A' <= x && x <= 'Z' -> "upper"
                    | otherwise            -> "other"

    -- using where
    caseOfFirstLetter''' :: String -> String
    caseOfFirstLetter''' "" = "empty"
    caseOfFirstLetter''' (x:xs)
        | inRange 'a' 'z' = "lower"
        | inRange 'A' 'Z' = "upper"
        | otherwise = "other"
        where
            inRange lower upper = lower <= x && x <= upper