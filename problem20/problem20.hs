-- (*) Remove the K'th element from a list.

-- Index starts at 0
removeAt :: Int -> [a] -> (Maybe a,[a])
removeAt k xs
    | k < 0 || k >= length xs = (Nothing, xs)
    | otherwise = (Just item, first ++ rest) 
    where first = take (k-1) xs
          (item:rest) = drop (k-1) xs

main = do
    putStr "removeAt 3 [2,3,4,5,6] should be (Just 5,[2,3,4,6]\nresult: "
    print $ removeAt 3 [2,3,4,5,6]
    putStr "removeAt -2 \"abcdefg\" should be (Nothing,\"abcdefg\")\nresult: "
    print $ removeAt (-2) "abcdefg"