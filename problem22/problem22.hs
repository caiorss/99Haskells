-- Create a list containing all integers within a given range.

module Problem22 where

range :: Int -> Int -> [Int]
range x y 
    | x == y = [y]
    | otherwise = x:(range (x+1) y)




main = do
    putStr "range 2 8 should be [2,3,4,5,6,7,8]\nresult: "
    print $ range 2 8
    putStr $ "range 7 86 should be "++(show [7..86])
    putStr "\nresult: "
    print $ range 7 86