-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no 
-- duplicates it is simply copied into the result list. Only elements with 
-- duplicates are transferred as (N E) lists.
module Problem11
( modRLE
, encodeMod
, ListElem (Single, Multiple)
) where 

import Problem10
-- having bugs with the main on this one... I can't figure out why

data ListElem a = Single a | Multiple Int a
    deriving (Show)

modRLE :: (Eq a) => [a] -> [ListElem a] 
modRLE = map magic . pack'
    where magic = (\x -> if (length x) == 1 then Single (head x) else Multiple (length x) (head x))

encodeMod :: (Eq a) => [a] -> [ListElem a]
encodeMod = map encHelp . runLengthEnc
	where encHelp (1,x) = Single x
	      encHelp (n,x) = Multiple n x

main = do
    mapM_ print [modRLE [1,1,1,1,2,2,2,3,4,5,5],[Multiple 4 1,Multiple 3 2,Single 3,Single 4,Multiple 2 5]]
    mapM_ print [modRLE "wwwwhhhheeeeeeee", [Multiple 4 'w',Multiple 4 'h',Multiple 8 'e']]
    mapM_ print [encodeMod [1,1,1,1,2,2,2,3,4,5,5],[Multiple 4 1,Multiple 3 2,Single 3,Single 4,Multiple 2 5]]
    mapM_ print [encodeMod "wwwwhhhheeeeeeee", [Multiple 4 'w',Multiple 4 'h',Multiple 8 'e']]
