-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flattenList :: NestedList a -> [a]
flattenList (List []) = []
flattenList (Elem x) = [x]
flattenList (List (x:xs)) = (flattenList x) ++ flattenList (List xs)

-- this one taught me a little more about user defined data types, data contructors, and type constructors

main = do
	print $ if [1,2,3,4,5] == flattenList (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) then "YAY" else "NAY"