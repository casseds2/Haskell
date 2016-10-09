import Data.List

--Take elements form a list so they evaluate to a given integer
balancer :: Int -> [Int] -> [Int]
balancer 0 [] = []
balancer i [] = []
balancer i xs = if i == (head xs)   --I is a given int
                   then [head xs]
                else if((head xs) < i)
                        then (head xs) : balancer(i - (head xs)) (tail xs)
                     else balancer i (tail xs)
--In the rest of the planned code, i would have been evaluated by halfSum :: [Int] -> Int