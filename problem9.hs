-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
import Control.Applicative

-- This one was pretty tough... it took me 45 mins! 
-- I finally got out some scratch paper and reasoned it out quickly--
-- initially I thought I had the right idea, but I didn't :(
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = pck [] (x:xs)
    where pck [] (y:ys) = pck [y] ys
          pck acc [] = [acc]
          pck (a:as) (y:ys) 
	          | a == y = pck (y:a:as) (ys)
	          | otherwise = (a:as):pack (y:ys)

-- an easier implementation from haskell.org
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = let (group,rest) = span (==x) xs
               in (x:group):(pack' rest)

main = do
	print $ pack [1,1,1,1,2,2,2,3,4,4,4,4,5,5,6,6]
	print $ pack' [1,1,1,1,2,2,2,3,4,4,4,4,5,5,6,6]