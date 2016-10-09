import Data.Array
import Data.List
import Data.Ord
addMe :: Int -> Int -> Int
addMe x y = x + y

addAny x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge :: Int -> String
whatAge x = "You daredevil you"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
--factorial 3 = 3 * 2
--factorial	2(ie n-1) = 2 * 1
--factorial 1(ie n-1) = 2 * 1
--factorial 0 = 1....base case kicks in

prodFactorial n = product [1..n] --same as above but done differently

isOdd :: Int -> Bool
isOdd n
	| n `mod` 2 == 0 = False
	| otherwise = True
	
isEven n = n `mod` 2 == 0 --same as above dne differently

whatGrade :: Int -> String
whatGrade x 
	| (x >= 0) && (x <= 18) = "Youth"
	| otherwise = "Adult"
	
batAvg :: Double -> Double -> String
batAvg hits atBats
	| avg <= 0.2 = "Terrible Average"
	| avg <= 0.25 = "Average Player"
	| avg <= 0.28 = "Very good"
	| otherwise = "All star"
	where avg = hits / atBats
	
getListItems :: [Int] -> String
getListItems [] = "Empty List"
getListItems (x:[]) = "Your list begins with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "Your list conatins " ++ show x ++ " first, and then " ++ show xs
   
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ show all ++ " is " ++ show x

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

mulBy4 ::[Int] -> [Int]
mulBy4 [] = []
mulBy4 (x:xs) = times4 x : mulBy4 xs

{-[1,2,3,4] : x = 1 | xs = [2,3,4]
[2,3,4] : x = 2 | xs = [3,4]
[3,4] : x = 3 | xs = [4]
[4] : x = 4 | xs = []
[] : x = [] base case so end-}

areStringsEqual :: [Char] -> [Char] -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _ = False

triangleArea :: Float -> Float -> Float -> Float
triangleArea x y z = sqrt(s * (s - x) * (s - y) * (s - z)) where s = (x + y + z) / 2

sumTest :: Int -> Int -> Int -> Bool
sumTest x y z
	| x + y == z = True
	| x + z == y = True
	| y + z == x = True
	| otherwise = False
	
validTriangle :: Float -> Float -> Float -> Bool
validTriangle x y z
	| x + y < z = False
	| x + z < y = False
	| z + y < x = False
	| otherwise = True

errValid :: Float -> Float -> Float -> Float
errValid x y z = if (validTriangle x y z)
					then triangleArea x y z
					else error "Not a real Triangle!"
					
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = if a == reverse a
					then True
				else False
				
shortest :: [[a]] -> [a]
shortest [a]= a
shortest (x:xs) = if length x < length (shortest xs)
					then x
					else	shortest xs
					
type Poly = [Float]
addPolys :: Poly -> Poly -> Poly
addPolys [] p = p
addPolys p [] = p
addPolys (p:ps) (q:qs) = (p+q):(addPolys ps qs)

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (y:ys) xs = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [] = error "EmptyList"
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "EmptyList"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "EmptyList"
myTail (_:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "EmptyList"
myInit [a] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: Num a  => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "EmptyList"
myMaximum [a] = a
myMaximum (x:xs) = if x > (myMaximum xs)
					then x
					else myMaximum xs
					
myMinimum :: Ord a => [a] -> a
myMinimum [] = error "EmptyList"
myMinimum [a] = a
myMinimum (x:xs) = if x < (myMinimum xs)
					then x
					else	myMinimum xs
					
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y
					then True
					else	myElem x ys
					
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys) = if x == y
					then ys
					else y:(myDelete x ys)
					
myUnion :: (Eq a) => [a] -> [a] -> [a]
myUnion xs [] = xs
myUnion [] xs = xs
myUnion xs (y:ys) = if (myElem y xs) || (myElem y ys) --if present in both
					then myUnion xs ys --continue on with rest of the list, ie. get rid of first element
					else myUnion (myAppend xs [y]) ys --join element y to it and continue through myUnion
					
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect(x:xs) ys = if myElem x ys
						then x:(myIntersect xs ys)
						else myIntersect xs ys
						
{-}data BinTree t = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))
						(Root 7 Empty Empty)
leaf x = Root x Empty Empty
myTree = Root 5 (Root 1 (Empty) (Leaf 3))
                (Leaf 7)-}

				
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

fibonacci :: Integer -> Integer
fibonacci n = memo!n
   where
   memo = array (0, n) [ (i, fib i) | i <- [0..n] ]
   fib 0 = 0
   fib 1 = 1
   fib i = memo!(i-1) + memo!(i-2)



   
funcX :: Fractional a => [a] -> a
funcX [] = 0
funcX [a] = a
funcX (x:xs) = (x + funcX(xs))

halfSum :: Fractional a => [a] -> a
halfSum [] = 0
halfSum [a] = a / 2
halfSum x = (funcX x) / 2

sumX :: Num a => [a] -> a
sumX [] = 0
sumX [a] = a
sumX (x:xs) = x + (sumX xs)

part ::Num a => Eq a =>  [a] -> [([a], [a])]
part [] = [([], [])]
part [a] = [([a], [])]
part (x:xs) = if x == sumX xs
              then [([x], xs)]
              else x:(part xs)

parts :: [a] -> [a]
parts n = memo!n
where
 memo = array(x,n)[(i,par i) | i <- x]
















	


	