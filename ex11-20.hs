--https://wiki.haskell.org/99_questions/11_to_20

--10 : Implment run length encoding
rle :: Eq a => [a] -> [(Int, a)]
rle xs = rle_aux 1 xs
    where
        rle_aux _ [] = error "empty list"
        rle_aux n (x:[]) = (n,x) : []
        rle_aux n (x:y:xs) | x == y = rle_aux (n+1) (y:xs)
                           | otherwise = (n,x) : (rle_aux 1 $ y:xs)

--11 : rle modified
data Elem a = Multiple Int a | Single a
        deriving (Show)

rlem :: Eq a => [a] -> [Elem a]
rlem xs = rlem_aux $ rle xs
        where
            rlem_aux ([]) = []
            rlem_aux ((1,x):ls) = (Single x) : (rlem_aux ls)
            rlem_aux ((n,x):ls) = (Multiple n x) : (rlem_aux ls)
            
--12 : rle decode from the es11 encoding 
rlem_decode :: [Elem a] -> [a]
rlem_decode [] = []
rlem_decode ((Single c):xs) = c : (rlem_decode xs)
rlem_decode ((Multiple n c):xs) = (take n $ repeat c) ++ (rlem_decode xs)

--14 : duplicate every element of the list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--15: replicate every element the given number of times
repli :: [a] -> Int -> [a]
repli xs n = repli_aux xs n n
            where
                repli_aux [] _ _ = []
                repli_aux (x:xs) 1 n = x : (repli_aux xs n n)
                repli_aux (x:xs) c n = x : (repli_aux (x:xs) (c-1) n)

--16: drop every n'th element of a list
dropEvery :: Integral b => [a] -> b -> [a]
dropEvery xs n = dropEvery_aux xs n n
                where
                    dropEvery_aux [] _ _ = []
                    dropEvery_aux (x:xs) 1 n = dropEvery_aux xs n n
                    dropEvery_aux (x:xs) c n = x : (dropEvery_aux xs (c-1) n)

--17: split a list in two lists, without predefined functions
split :: [a] -> Int -> ([a],[a])
--split xs n = ((take n xs), (drop n xs))
split xs n = split_aux ([],xs) n
            where
                split_aux (a,b) 0 = (reverse a, b)
                split_aux (a, (x:xs)) n = split_aux ((x:a),xs) (n-1)

--18: extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs a z = drop (a-1) $ take z xs

--19: rotate a list
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0  = xs
rotate (x:xs) n | n > 0 = rotate (xs++[x]) (n-1)
rotate xs n | n < 0 = reverse $ rotate (reverse xs) (-1*n)

--20: remove the n-th element of a list and return the removed element and the result list
removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = removeAt_aux n [] xs
                where
                    removeAt_aux 0 _ _ = error "index must be >= 1"
                    removeAt_aux 1 acc (x:xs) = (x, (reverse acc)++xs)
                    removeAt_aux n acc (x:xs) = removeAt_aux (n-1) (x:acc) xs