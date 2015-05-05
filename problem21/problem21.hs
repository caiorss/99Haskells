-- 1 Problem 21
-- Insert an element at a given position into a list.
module Problem21 where

insertAt :: a -> [a] -> Int -> [a]
insertAt i [] _ = [i]
insertAt i (l:ls) pos
    | pos == 1 = i:l:ls
    -- | pos < 1  = l:ls
    | otherwise = l:(insertAt i ls $ pos-1)

main = do
    putStr "insertAt 9 [2,3,4,5,6] 2 should be [2,9,3,4,6]\nresult: "
    print $ insertAt 9 [2,3,4,5,6] 2
    putStr "insertAt 'z' \"abcdefg\" 7 should be \"abcdefzg\")\nresult: "
    print $ insertAt 'z' "abcdefg" 7