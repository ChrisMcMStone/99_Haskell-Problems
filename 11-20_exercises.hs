--11) Modified run-length encoding.
--    Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
--    Only elements with duplicates are transferred as (N E) lists.

import 1-10_exercises (pack, encode)

data Encode a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encode a]
encodeModified [] = []
encodeModified xs = let list = List.group xs in
                    foldr(\x acc -> let len = length x in
                                    if len == 1 then (Single $ head x) : acc
                                    else (Multiple len $ head x) : acc ) [] list
                                    
-- encodeModified2 :: (Eq a) => [a] -> [Encode a]
-- encodeModified2 xs = map modify $ encode xs where
--                      modify xs
