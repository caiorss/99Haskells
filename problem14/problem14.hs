-- (*) Duplicate the elements of a list.

dupList :: [a] -> [a]
dupList [] = []
dupList (x:xs) = x:x:dupList xs

main = do
    print [1,1,2,2,3,3,4,4,5,5]
    print $ dupList [1,2,3,4,5]
    print "HHaasskkeelleekkssaaHH"
    print $ dupList "HaskeleksaH"
