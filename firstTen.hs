import qualified Data.List as List

--1) Find last element of a list
--
myLast :: [a] -> a
myLast = last

--myLast2 :: [a] -> a
--myLast2 = head . reverse

myLast3 :: [a] -> a
myLast3 xs = xs !! length xs

--2) Find the last but one element of a list
--
myButLast :: [a] -> a
myButLast = last . init

myButLast2 :: [a] -> a
myButLast2 xs = xs !! (length xs - 1)

myButLast3 :: [a] -> a
myButLast3 = head . tail . reverse

--3) Find the K'th element of a list (The first element in the list is 1)
--
elementAt :: [b] -> Int -> b
elementAt _ 0 = error "index out of bounds"
elementAt xs k = xs !! (k - 1)

elementAt2 :: [b] -> Int -> b
elementAt2 [] _ = error "index out of bounds"
elementAt2 _ 0 = error "index out of bounds"
elementAt2 (x:_) 1 = x
elementAt2 (_:xs) k = elementAt2 xs k

elementAt3 :: [b] -> Int -> b
elementAt3 xs k = last . take k $ xs 

--4) Find the number of elements in a list
--
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength2 :: [a] -> Int
myLength2 = List.foldl'(\acc _ -> acc + 1) 0 

myLength3 :: [a] -> Int
myLength3 = sum . map (const 1)

--5) Reverse a list.
--
myReverse :: [a] -> [a]
myReverse = reverse

myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

myReverse3 :: [a] -> [a]
myReverse3 [] = []
myReverse3 [x] = [x]
myReverse3 (x:xs) = myReverse3 xs ++ [x]

--6) Find out whether a list is a palindrome.
--
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 [] = True
isPalindrome2 [_] = True
isPalindrome2 xs = head xs == last xs && isPalindrome2 (tail . init $ xs)

--7) Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x

flatten2 :: NestedList a -> [a]
flatten2 (Elem a) = [a]
flatten2 (List xs) = foldr (\x acc -> flatten2 x ++ acc) [] xs

flatten3 :: NestedList a -> [a]
flatten3 (Elem a) = [a]
flatten3 (List []) = []
flatten3 (List (x:xs)) = flatten3 x ++ flatten3 (List xs)

--8) Eliminate consecutive duplicates of list elements (order of elimates should not change)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs@(y:_)) | x == y = compress xs 
                      | otherwise = x : compress xs

compress2 :: (Eq a) => [a] -> [a]
compress2 xs = foldr (\x acc -> if x == head acc then acc else x:acc) [last xs] xs

compress3 :: (Eq a) => [a] -> [a] 
compress3 xs = dropWhile (\a -> a == head xs) xs

compress4 :: (Eq a) => [a] -> [a]
compress4 xs = concatMap (take 1) $ List.group xs

compress5 :: (Eq a) => [a] -> [a]
compress5 = map head . List.group

compress6 :: (Eq a) => [a] -> [a]
compress6 [] = []
compress6 (x:xs) = x : compress6 (dropWhile (== x) xs)

--9) Pack consecutive duplicates of list elements into sublists.

pack :: (Eq a) => [a] -> [[a]]
pack = List.group

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 l@(x:xs) = takeWhile (== x) l : pack2 (dropWhile (== x) xs)

pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
pack3 l@(x:_) = let result = span (== x) l in
                fst result : pack3 (snd result)

--10) Run-length encoding of a list. Use the result of Q9 to implement the so-called run-length encoding data compression method.
--Consecutive duplicates of elements are encoded as lists (N, E) where N is the number of duplicates of the element E. 

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = let packedList = pack xs in
        zip (map length packedList) (map head packedList)

encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 = map (\x -> (length x, head x)) . pack
