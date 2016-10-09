import Data.List

--Passes two arguments to solve the partition problem
partitionSolver :: [Int] -> [Int]
partitionSolver [] = []
partitionSolver xs = headPart(target xs) (reverse(sort xs))

--Solves partition problem if head is greater than half of sum
headPart :: Int -> [Int] -> [Int]   --takes an int and a list...target int and the list it came from
headPart 0 xs 	      = []   
headPart a [] 	      = []
headPart a xs
	| a == (head xs) = [head xs]
	| otherwise 	 = bodyPart xs

--Returns one side of the partition
bodyPart xs  = 	if (head xs < (target xs))   --if first value in list is less than the target value to split the list into
                                then head xs : headPart ((target xs) - head xs) (tail xs)	
                            else headPart (target(xs)) (tail xs)
								
--Sum of a List of Numbers
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

--Calculates the target sum of the first partition
target :: [Int] -> Int         
target []   = 0
target xs   = if (sumList(xs) `mod` 2 == 0)   --if the list evaluates to an even number
                 then (sumList(xs)) `div` 2   --return an integer of half the sum of the list...a target value to partition to
              else ((sumList(xs)) `div` 2) + 1   --else calculate the nearest best number to works towards...next even number

--Returns the Two Balanced Partitions    
balancedPartition :: [Int] -> ([Int],[Int])
balancedPartition []    = ([],[])
balancedPartition xs = (partitionSolver xs, (xs \\ partitionSolver xs))
