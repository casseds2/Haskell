import Data.List

--Balances a list to evaluate to a given integer
balancer :: Int -> [Int] -> [Int]   --Take an int (half sum of list) and a list as paramteres...return a list
balancer 0 [] = []
balancer x [] = []
balancer a xs  = if a == (head xs)  --if getHalf == head of xs
                    then [head xs]   --then return the head of xs in list form
                 else if ((head) xs < a)   --otherwise if head value is less than getHalf
                         then (head xs) : balancer (a - (head xs)) (tail xs)   --append the head to the start of the list and getHalf is re-evaluated and the list becomes its tail...recursively call balance on the new getHalf and remainder of the list
                      else balancer a (tail xs)   --otherwise recursively call balance on the rest of the list
