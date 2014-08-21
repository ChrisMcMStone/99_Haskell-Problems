import FirstTen (pack, encode)

--11) Modified run-length encoding.
--    Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
--    Only elements with duplicates are transferred as (N E) lists.

data Encode a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encode a]
encodeModified [] = []
encodeModified xs = let list = FirstTen.pack xs in
                    foldr(\x acc -> let len = length x in
                                    if len == 1 then (Single $ head x) : acc
                                    else Multiple len (head x) : acc ) [] list
                                    
encodeModified2 :: (Eq a) => [a] -> [Encode a]
encodeModified2 xs = modify $ encode xs where
                     modify = foldr (\(n, x) acc -> if n == 1 then Single x : acc else
                                                                 Multiple n x : acc) []

encodeModified3 :: (Eq a) => [a] -> [Encode a]
encodeModified3 = map change . FirstTen.encode where
        change :: (Int, a) -> Encode a
        change (1, x) = Single x
        change (n, x) = Multiple n x
