import qualified Data.List as List 


-- Transposing a vector

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [[x], [y]] = [[x, y]]
transpose xs = map head xs : transpose (map tail xs)


-- Quicksort with list comprehensions

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
         let biggerPart = quicksort [ a | a <- xs, a > x] in
         let smallerPart = quicksort [ a | a <- xs, a <= x] in
         smallerPart ++ [x] ++ biggerPart


-- Searching for sublist within a list with foldr

searchSub :: (Eq a) => [a] -> [a] -> Bool
searchSub xs ys = foldr (\x acc -> (take listLen x == xs) || acc) False (List.tails ys) 
                where listLen = length xs 

-- Key, value pair search using Maybe data type

searchKey :: (Eq k) => k -> [(k, v)] -> Maybe v
searchKey k = foldr (\(x, y) acc -> if x == k then Just y else acc) Nothing 
