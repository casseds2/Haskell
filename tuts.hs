myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend (y:ys) xs = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [x] = x
myHead (x:xs) = x

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "Empty List"
myTail (x:xs) = xs

myInit :: [a] -> [a]
mtInit [] = error "Empty List"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Empty List"
myMaximum [x] = x
myMaximum (x:xs) = if x > (myMaximum xs)
                   then x
				   else myMaximum xs

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty List"
myMinimum [x] = x
myMinimum (x:xs) = if x < (myMinimum xs)
                   then x
				   else myMinimum xs
				
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y
                  then True
                  else myElem x ys				  

myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys) = if x == y
                    then ys
					else y:(myDelete x ys)

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion xs [] = xs
myUnion xs (y:ys) = if(myElem y xs || myElem y ys)
                    then myUnion xs ys
					else myUnion(myAppend xs [y]) ys

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect (y:ys) xs = if myElem y xs
                        then y:(myIntersect xs ys)
						else myIntersect xs ys
						
myPalindrome :: Eq a => [a] -> Bool
myPalindrome x = if x == reverse x
                 then True
				 else False
				 
shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
                  then x
				  else shortest xs

type Poly = [Float]
sumPoly :: Poly -> Poly -> Poly
sumPoly [] x = []
sumPoly x [] = []
sumPoly (x:xs) (y:ys) = (x + y):(sumPoly xs ys)

evalPoly :: Poly -> Float -> Float
evalPoly [p] _ = p
evalPoly (p:ps) x = p + (x * (evalPoly ps x))

rightTri :: (Int, Int, Int) -> Bool
rightTri (x,y,z) = if x*x == y*y + z*z
                      then True
                      else
				          if y*y == x*x + z*z
                              then True
                              else
				                 if z*z == x*x + y*y
					                then True
				                    else
				                       False
								
right :: (Int, Int, Int) -> Bool
right (x,y,z)
   |x*x == y*y + z*z = True
   |y*y == x*x + z*z = True
   |z*z == x*x + y*y = True
   |otherwise = False
								
			








