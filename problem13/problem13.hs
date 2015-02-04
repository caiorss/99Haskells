
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression 
-- method directly. I.e. don't explicitly create the sublists 
-- containing the duplicates, as in problem 9, but only count them. 
-- As in problem P11, simplify the result list by replacing the 
-- singleton lists (1 X) by X.


data ListElem a = Single a | Multiple Int a
    deriving (Show)

encodeDirect :: (Eq a) => [a] -> [ListElem a]
encodeDirect [] = []
encodeDirect (x:xs) = countElems x 1 xs
    where countElems e c [] = [encodeElement c e]
    	  countElems e c (y:ys)
              | e == y = countElems e (c+1) ys
              | otherwise = (encodeElement c e):(countElems y 1 ys)

-- I liked this helper function from Haskell.org so I modified
-- my solution to fit it in. I esentially had this same code, just
-- inserted into the encodeDirect function
encodeElement :: Int -> a -> ListElem a
encodeElement 1 x = Single x
encodeElement n x = Multiple n x

main = do
    mapM_ print [encodeDirect [1,1,1,1,2,2,2,3,4,5,5],[Multiple 4 1,Multiple 3 2,Single 3,Single 4,Multiple 2 5]]
    mapM_ print [encodeDirect "wwwwhhhheeeeeeee", [Multiple 4 'w',Multiple 4 'h',Multiple 8 'e']]