-- (**) Replicate the elements of a list a given number of times.
import Control.Monad

repli :: (Enum a) => [a] -> Int -> [a]
repli xs n = xs >>= (\x -> take n $ [x,x..])

main = do
	putStrLn"Testing [1,2,3,4,5] 3"
	putStrLn "Answer should be:\n       [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]"
	putStrLn $ "Result:" ++ (show $ repli [1,2,3,4,5] 3)
