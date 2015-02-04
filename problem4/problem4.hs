-- tail recursive (faster)
myLength :: [a] -> Int
myLength [] = 0
myLength zs = myLen zs 0
	where myLen [] acc = acc
	      myLen (x:xs) acc = myLen xs (acc+1)

-- slow version
myLength' :: [a] -> Int
myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

main = do
	let t1 = [1,2,3,4,5,6]
	    t2 = [4,5,6,7,8,9,0,2]
	    t3 = [1,1,2,3,5]
	    t4 = []
	    t5 = [2000000]
	    passed = [6,8,5,0,1] == map myLength [t1,t2,t3,t4,t5]
	putStrLn $ if passed then "Tests passed!" else "Failed tests!"