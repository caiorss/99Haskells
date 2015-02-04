-- reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = rev xs []
	where rev [] rl = rl
	      rev (x:xs) rl = rev xs (x:rl)

main = do
	let test = [1,2,3,4,5,6,7,8,9]
	    passed = myReverse test == [9,8,7,6,5,4,3,2,1]
	putStrLn $ if passed then "Tests passed!" else "Failed tests!"