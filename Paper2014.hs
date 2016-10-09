
------------------------------------- Q1 ----------------------------------------------
union :: (Eq a) => [a] -> [a] -> [a]
union [] a = a
union a [] = a
union x (y:list) = if elem y x
					then union x list
					else y : (union x list)

inter :: (Eq a) => [a] -> [a] -> [a]
inter a [] = []
inter [] a = []
inter x (y:list) = if elem y x
					then y : (inter x list)
					else inter x list

--------------------------------------------Q2 ---------------------------------------

data Bintree t = Empty | Root Int (Bintree t) (Bintree t)	
							deriving (Ord, Eq, Show)


leaf x = Root x Empty Empty
-----------------------------------------------------------

{--insert :: Int -> Bintree t -> Bintree t
insert a Empty = leaf a	
insert a (Root x left right)
	| a < x = Root x (insert a left) right
	| otherwise = Root x left (insert a right)					

list :: Bintree t -> [Int]
list Empty = []
list (Root x left right) = list left ++ [x] ++ list right

avg :: [Int] -> Int
avg [] = 0
avg (x:list) = ((x + (avg list))) / (length (x : list))

average :: Bintree t -> Int
average (Root x left right) = avg (list (Root x left right))--}


difference :: Eq a => [a] -> [a] -> [a]
difference a [] = []
difference [] a = []
difference (x:xs) y = if elem x y
                         then difference xs y
					  else
					     x : (difference xs y)

quick :: Ord a => [a] -> [a]
quick [] = []
quick (p:ps) = (quick lesser) ++ [p] ++ (quick greater)
			     where
			       lesser = filter (< p) ps
				   greater = filter (> p) ps



























