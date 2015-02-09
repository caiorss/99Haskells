-- (**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the
-- elements between the i'th and k'th element of the original list 
-- (both limits included). Start counting the elements with 1.

slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = error "Empty list can't be sliced"
slice i k xs = counter 1 xs
    where counter _ [] = []
    	  counter j (y:ys) 
              | j < i = counter (j+1) ys
              | j == k = [y]
              | otherwise = y:counter (j+1) ys

main = do
	putStr $ "slice 2 5 [1,3,5,7,9,11] == "
	print $ slice 2 5 [1,3,5,7,9,11]
	print "                  correct: [3,5,7,9]"