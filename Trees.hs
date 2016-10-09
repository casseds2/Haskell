data BinTree t = Empty | Root Int (BinTree t)(BinTree t)
                 deriving (Eq, Ord, Show)
myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))
                (Root 7 Empty Empty)

leaf x = Root x Empty Empty

addNode :: Int -> BinTree t -> BinTree t
addNode a Empty = leaf a
addNode x (Root a left right)
   | x < a = Root a (addNode x left) right
   | otherwise = Root a left (addNode x right)
   
makeTree :: [Int] -> BinTree t
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)

inorder :: BinTree t -> [Int]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right

mpsort :: [Int] -> [Int]
mpsort x = inorder (makeTree x)

hoAddNode :: (Int -> Int -> Bool) -> Int -> BinTree t -> BinTree t
hoAddNode _ a Empty = leaf a
hoAddNode fn x (Root a left right)
   | fn x a = Root a (hoAddNode fn x left) right
   | otherwise = Root a left (hoAddNode fn x right)
   
hoMakeTree :: (Int -> Int -> Bool) -> [Int] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)

{--hosort :: (Int -> Int -> Bool) -> [Int] -> [Int]
hsort fn x = inorder (hoMakeTree fn x)--}

shortPath :: BinTree t -> Int
shortPath Empty = 0
shortPath (Root _ left right) = 1 + (min(shortPath left)(shortPath right))

list :: BinTree t -> [Int]
list Empty = []
list (Root x left right) = list left ++ [x] ++ list right



monkeyMake :: [Int] -> BinTree t
monkeyMake [] = Empty
monkeyMake (x:xs) = monkeyNode x (monkeyMake xs)

monkeyNode :: Int -> BinTree t -> BinTree t
monkeyNode a Empty = leaf a
monkeyNode x (Root a left right)
         | x < a = Root a (monkeyNode x left) right
		 |otherwise = Root a left (monkeyNode x right)
		 
monkeyOrd :: BinTree t -> [Int]
monkeyOrd Empty = []
monkeyOrd (Root x left right) = monkeyOrd right ++ [x] ++ monkeyOrd left

monkeySort :: [Int] -> [Int]
monkeySort x = monkeyOrd (makeTree x)

























