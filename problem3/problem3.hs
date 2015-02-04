elementAt :: [a] -> Int -> a
elementAt xs n = elemAt xs 1
	where elemAt [] i = error $ (show i)++" does not exist in that list!"
	      elemAt (x:xs) i 
	          | i == n = x
	          | otherwise = elemAt xs (i+1)

main = do
	let t1 = elementAt [1,2,3,4] 1
	    t2 = elementAt "abcd" 2
	    t3 = elementAt ["Learn","Haskell","For","Great","Good"] 3
	    t4 = elementAt [[1,2,3],[4,5,6],[7,8,9],[10]] 4
	    passed = (1,'b',"For",[10]) == (t1,t2,t3,t4)
	putStrLn $ if passed then "Tests passed!" else "Failed tests!"

