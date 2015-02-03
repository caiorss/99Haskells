-- Run-length encoding of a list. Use the result of problem Problem 9
-- to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N 
-- is the number of duplicates of the element E.
module Problem10 
( pack'
, runLengthEnc
) where

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = let (group,rest) = span (==x) xs
               in (x:group):(pack' rest)

-- this was quite easy
runLengthEnc :: (Eq a) => [a] -> [(Int,a)]
runLengthEnc = map (\x -> (length x, head x)) . pack' 

main = do
	print $ runLengthEnc [1,1,1,1,2,2,2,3,4,4,4,4,5,5,6,6]
	print $ runLengthEnc [1,1,1,1,2,2,2,3,4,4,4,4,5,5,6,6] == [(4,1),(3,2),(1,3),(4,4),(2,5),(2,6)]
	print $ runLengthEnc "wwwwhhhheeeeeeee" 
	print $ runLengthEnc "wwwwhhhheeeeeeee" == [(4,'w'),(4,'h'),(8,'e')]

