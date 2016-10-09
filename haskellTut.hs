import Data.List
import System.IO

maxInt = minBound :: Int
always10 :: Int
always10 = 10
sumOfNums = sum [1..1000]

num9 = 9 ::Int
srtqOf9 = sqrt (fromIntegral num9)

primes = [3,5,7,5,7,9,911,13]

favNums = 2 : 4 : 4 : 7 : 9 : [] --adds all to list


take3 = take 1 primes
drop3 = drop 1 primes

is3In = 3 `elem` primes

evenList = [2,4..100]

listTimes2 = [x * 2 | x <- [1,4..20], x * 2 <= 14] --[mltiply by 2, where x  is any even from 2 -20, if x *2 id less than 14]

powList = [3^n | n <- [1..5]]

mulTable = [[x * y | x <- [1..5]] | y <- [1..5]]

{--main = do
	putStrLn "Name?"
	name <- getLine
	putStrLn("Hello " ++ name)--}
	