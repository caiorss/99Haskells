-- (**) Drop every N'th element from a list.
groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = helper n xs []
	where helper _ [] rest = [rest]
	      helper i (y:ys) acc
	          | i == n = (reverse acc) : helper 1 ys [y]
	          | otherwise = helper (i+1) ys (y:acc)

dropevery :: Int -> [a] -> [a]
dropevery n xs = concat . map init' . groupsOf n $ xs
    where init' = (\x -> if length x == n then init x else x)

    -- this one was quite fun! I decided to come up with my own
    -- "chunksOf" function from Data.List.Split and that added a bit of challenge

main = do
    putStr $ "dropevery 2 [1,2,3,4,5,6,7,8,9,10,11] == "
    print $ dropevery 2 [1,2,3,4,5,6,7,8,9,10,11]
    print "                               correct: [1,3,5,7,9,11]"
    putStr $ "dropevery 3 \"abcdefghik\" == " 
    print $ dropevery 3 "abcdefghik"
    print $ "                   correct: abdeghk"