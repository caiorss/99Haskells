-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated 
-- as specified in problem 11. Construct its uncompressed version.

import Problem11

decode :: (Eq a) => [ListElem a] -> [a]
decode = (concatMap decHelp)
	where decHelp (Single x) = [x] 
	      decHelp (Multiple n x) = take n $ repeat x

main = do
    print $ decode [Multiple 4 1,Multiple 3 2,Single 3,Single 4,Multiple 2 5] == [1,1,1,1,2,2,2,3,4,5,5]
    print $ decode [Multiple 4 'w',Multiple 4 'h',Multiple 8 'e'] == "wwwwhhhheeeeeeee"
    print $ decode $ encodeMod "wwwwhhhheeeeeeee" 
 
 -- This one is (**), but I was proud that I found this to be really easy!
 --		> however, I did receive some help from the solution of Problem11 on the Haskell site
 -- 	> ... with the data ListElem declaration. New datatypes are fun!