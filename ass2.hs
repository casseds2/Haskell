import Data.Array
import Data.List
import Data.Ord --(comparing)

fibonacci :: Integer -> Integer
fibonacci n = memo!n
   where
   memo = array (0, n) [ (i, fib i) | i <- [0..n] ]
   fib 0 = 0
   fib 1 = 1
   fib i = memo!(i-1) + memo!(i-2)

--Working partition function
partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x : xs) = let ps = partitions xs in
    [(x : ys, zs) | (ys, zs) <- ps] ++ [(ys, x : zs) | (ys, zs) <- ps]

unbalance :: Num a => ([a], [a]) -> a
unbalance (ys, zs) = abs (sum ys - sum zs)

balancedPartition :: (Num a, Ord a) => [a] -> ([a], [a])
balancedPartition = minimumBy (comparing unbalance) . partitions
------------------------------------------------------------------------------------------------------------------------------------------------- 
--Separates a List of Integers Into All Possible Combinations as Sub Lists
part :: [a] -> [([a], [a])]   --take a list and return two sub lists
part [] = [([], [])]   --base case
part (x:xs) = [(x:ys,zs) | (ys,zs) <- part(xs)] ++ [(ys, x:zs) | (ys,zs) <- part(xs)]   --returns list of all possible combinations of the elements
   
--Sum of a List of Numbers
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

--Returns Difference Between Two of the Sub Lists
compareValue :: Num a => Ord a => ([a], [a]) -> a   --take two subsets and return the difference, ie. an int value
compareValue (xs, ys) = if sumList(xs) < sumList(ys)
                           then sumList(ys) - sumList(xs)   --{
                        else                                --Satisfy case in case one is bigger than the other
                           sumList(xs) - sumList(ys)        --}

--Find the Best Out of the Sub Lists
findBest :: Num a => Ord a => [a] -> ([a], [a])   --take a list and return the best sub list
findBest = minimumBy(comparing compareValue).part   --minimumBy comes with Data.List and finds smallest object relative to the comparison function (), ie. 'compareValue'....part is just calling 'part'

--Find the Two Sub Lists with the Least Difference
findLeastDifference :: Num a => Ord a => [([a], [a])] -> ([a], [a])   --take a list with sub lists and return the best sub list to fit specified case
findLeastDifference = minimumBy(comparing compareValue)  --minimumBy comes with Data.List and finds smallest object relative to the comparison function (https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html), ie. 'compareValue'

--Bring All Parts Together to Find Sub List with Least Difference After Partitioning the Original List
balancePartition :: Ord a => Num a => [a] -> ([a], [a])   --take a list and return a sub list
balancePartition xs = findLeastDifference(part xs)   --find the sub lists with least difference after partitioning it

--Efficient Fibonacci Memoisation function
effFib ::Integer -> Integer
effFib n = memo!n
   where
   memo = array (0,n)[(i, fib i) | i <- [0..n]]
   fib 0 = 0
   fib 1 = 1
   fib n = memo!(n-2) + memo!(n-1)



