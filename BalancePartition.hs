import Data.List -- minimumBy comes from this library
import Data.Ord -- comparing comes from this library

--Separates a List of Integers Into All Possible Combinations as Sub Lists--
part :: [a] -> [([a], [a])]   --take a list and return two sub lists
part [] = [([], [])]   --base case
part (x:xs) = [(x:ys,zs) | (ys,zs) <- part(xs)] ++ [(ys, x:zs) | (ys,zs) <- part(xs)]   --returns list of all possible combinations of the elements with list comprehension
   
--Sum of a List of Numbers--
sumList :: Num a => [a] -> a   --take a list and return a value, ie. integer value
sumList [] = 0   --if its empty && base case
sumList (x:xs) = x + sumList(xs)   --recursive case

--Returns Difference Between Two of the Sub Lists--
compareValue :: Num a => Ord a => ([a], [a]) -> a   --take two subsets and return the difference, ie. an int value
compareValue (xs, ys) = if sumList(xs) < sumList(ys)
                           then sumList(ys) - sumList(xs)   --{
                        else                                --Satisfy case in case one is bigger than the other
                           sumList(xs) - sumList(ys)        --}

--Find the Two Sub Lists with the Least Difference--
findLeastDifference :: Num a => Ord a => [([a], [a])] -> ([a], [a])   --take a list with sub lists and return the best sub list to fit specified case
findLeastDifference = minimumBy(comparing compareValue)  --minimumBy comes with Data.List and finds smallest object relative to the comparison function (https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html), ie. 'compareValue'
                                                         --after looking up minimumBy, found it could be used in conjunction with 'comparing' from the Ord library to compare elements of a list

--Bring All Parts Together to Find Sub List with Least Difference After Partitioning the Original List--
balancePartition :: Ord a => Num a => [a] -> ([a], [a])   --take a list and return a sub list
balancePartition xs = findLeastDifference(part xs)   --find the sub lists with least difference after partitioning it

