-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

-- Function goes here:
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n (x:xs)
    | n < 0 = rotate (n+1) $ last xs : init (x:xs)
    | n > 0 = rotate (n-1) $ xs ++ [x]

main = do
    putStr "rotate 5 [2,3,4,5,6] should be [2,3,4,5,6]\nresult: "
    print $ rotate 5 [2,3,4,5,6]
    putStr "rotate -2 \"abcdefg\" should be \"fgabcde\"\nresult: "
    print $ rotate (-2) "abcdefg"