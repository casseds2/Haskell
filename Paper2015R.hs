-- 1(C)

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


--2(C)
allSquares :: [Integer]
allSquares = (filter even (map (^2) [1..100]))

headOfList :: [a] -> a
headOfList xs = case xs of [] -> error "Empty list!"
                        (x:_) -> x