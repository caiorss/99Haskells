isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome z@(x:xs) = (head z) == (last z) && (isPalindrome $ init xs)

main = do
	let t1 = isPalindrome [1,2,3,4,3,2,1]
	    t2 = isPalindrome "abcdefedcba"
	    t3 = isPalindrome ["I", "love", "Haskell", "love", "me"]
	    t4 = isPalindrome [2.5,3.5,2.5]
	print $ if (True, True, False, True) == (t1,t2,t3,t4) then "Tests passed!" else "Failed tests!"