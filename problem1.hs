myLast :: [a] -> a
myLast [] = error "Cannot find last element of an empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

main = do
	let t1 = myLast [1,2,3,4]
	    t2 = myLast "abcd"
	    t3 = myLast ["Learn","Haskell","For","Great","Good"]
	    t4 = myLast [[1,2,3],[4,5,6],[7,8,9],[10]]
	    passed = (4,'d',"Good",[10])== (t1,t2,t3,t4)
	putStrLn $ if passed then "Tests passed!" else "Failed tests!"