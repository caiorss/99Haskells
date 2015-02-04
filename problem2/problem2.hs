lastButOne :: [a] -> a
lastButOne (x:y:z:xs) = lastButOne (y:z:xs)
lastButOne (x:y:xs) = x
lastButOne _ = error "List must have two elements for lastButOne"

main = do
	let t1 = lastButOne [1,2,3,4]
	    t2 = lastButOne "abcd"
	    t3 = lastButOne ["Learn","Haskell","For","Great","Good"]
	    t4 = lastButOne [[1,2,3],[4,5,6],[7,8,9],[10]]
	    passed = (3,'c',"Great",[7,8,9])== (t1,t2,t3,t4)
	putStrLn $ if passed then "Tests passed!" else "Failed tests!"