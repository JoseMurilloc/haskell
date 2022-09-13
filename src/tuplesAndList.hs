-- List in haskell
otherList = 4:[]
list = [4]

listEnumerable = [2..10]
joinList = [1,2] ++ [2,2]

-- Size of list with recursion
length_list :: [t] -> Int
length_list [] = 0
length_list (x:xs) = 1 + length_list xs

-- Duplicate for each element of list
duplicate_list :: [Int] -> [Int]
duplicate_list list
  | list == [] = []
  | otherwise = [(head list) * 2] ++ duplicate_list (tail list)

-- 👺 pattern match Duplicate for each element of list
double_list :: [Int] -> [Int]
double_list [] = []
double_list (x:xs) = [x*2] ++ double_list xs

-- 👹 if one element is a list
belongs :: Int -> [Int] -> Bool
belongs item [] = False
belongs item (x:xs)
  | x == item = True
  | otherwise = belongs item xs   

-- 👹 sum all item is a list that pair
sum_pairs :: [Int] -> Int
sum_pairs (x:[])
  | mod x 2 == 0 = x
  | otherwise = 0
sum_pairs (x:xs) 
  | mod x 2 == 0 = x + sum_pairs xs
  | otherwise = 0 + sum_pairs xs

-- 👹 bigger element is a list
bigger_item_list :: [Int] -> Int
bigger_item_list [] = (-1)
bigger_item_list (x:[]) = x
bigger_item_list (x:xs) 
  | x > bigger_item_list xs = x
  | otherwise = bigger_item_list xs


-- 👺 list only numbers pairs of listing 
list_pairs :: [Int] -> [Int]
list_pairs [] = []
list_pairs (x:xs) = [x | x <- (x:xs), mod x 2 == 0]

-- Tuples

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Phone, Age)

getName :: Person -> String
getName (n,p, a) = n

