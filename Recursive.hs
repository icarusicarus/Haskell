module Recursive where
    fib :: Int -> Int
    fib 0 = 1
    fib 1 = 1
    fib n = fib(n-1) + fib(n-2)

    length' :: [a] -> Int
    length' [] = 0
    length' (x:xs) = 1 + length' xs

    take' :: Int -> [a] -> [a]
    take' n _ | n <= 0 = []
    take' _ []         = []
    take' n (x:xs)     = x : take' (n-1) xs

    -- insert sort
    ins :: Ord a => a -> [a] -> [a]
    ins e [] = [e]
    ins e (x:xs)
        | e < x     = e : x : xs
        | otherwise = x : ins e xs

    insSort :: Ord a => [a] -> [a]
    insSort [] = []
    insSort (x:xs) = ins x (insSort xs)