-- (*) Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> ([a],[a])
split xs n = build 0 ([],xs)
    where build c (zs,(y:ys))
              | c == n = (reverse zs,(y:ys))
              | otherwise = build (c+1) ((y:zs),ys)

main = do
	putStr $ "split [1,2,3,4,5,6] 3 == "
	print $ split [1,2,3,4,5,6] 0
