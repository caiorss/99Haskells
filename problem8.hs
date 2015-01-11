-- Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x:(comp xs x)
	where comp [] acc = []
	      comp [x] acc 
	          | x == acc = []
	          | otherwise = [x]
	      comp (x:xs) acc
	          | x == acc = comp xs acc
	          | otherwise = x:(comp xs x)
main = do
	print [1,2,3,4,5,6] 
	print $ compress [1,1,1,2,2,2,2,2,2,3,4,4,5,6,6,6,6,6,6,6,6]
	print $ if [1,2,3,4,5,6] == compress [1,1,1,2,2,2,2,2,2,3,4,4,5,6,6,6,6,6,6,6,6] then "Test passes!" else "Test fails!"