myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend (y:ys) xs = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [x] = x
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "EmptyList"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "EmptyList"
myTail [x] = []
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "EmptyList"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (y:ys) = y ++ (myConcat ys)

mySum :: Num a => [a] -> a
mySum [x] = x
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "EmptyList"
myMaximum [x] = x
myMaximum (x:xs) = if x > (myMaximum xs)
                   then x
				   else myMaximum xs

--listnum :: Int -> Int -> [Int]				   
listnum a b = [a..b]






