--https://wiki.haskell.org/99_questions/1_to_10

--1 : find last element of a list
myLast :: [a] -> a
myLast ([]) = error "empty list has no last element"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--2 : find last but one element of a list
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "list has not such element"

--3 : find k-th element
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "list starts with 1"
elementAt [] _ = error "index out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

--4 : find length list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--5 : reverse a list
myReverse :: [a] -> [a]
--myReverse' [] = []
--myReverse' (x:xs) = (myReverse' xs) ++ [x]
--myReverse xs = foldl (flip (:)) [] xs
myReverse xs = myReverse_aux [] xs 
    where
        myReverse_aux acc [] = acc
        myReverse_aux acc (x:xs) = myReverse_aux (x : acc) xs

--6 : find if list is palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome xs | a == z = isPalindrome $ tail $ init $ xs
    where a = head xs
          z = last xs
isPalindrome _ = False

--7 : flatten nested lists
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)

--8 : eliminate consecutive duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) | x == y = compress $ y:xs
                  | otherwise = x : (compress $ y:xs)

--9 : Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack xs = init $ pack_aux 1 xs
pack_aux _ [] = [[]]
pack_aux n (x:[]) = (take n $ repeat x) : [[]]
pack_aux n (x:y:xs) | x == y = pack_aux (n+1) (y:xs)
                    | otherwise = (take n $ repeat x) : (pack_aux 1 $ y:xs)

--10 : Implment run length encoding
rle :: Eq a => [a] -> [(Int, a)]
rle xs = rle_aux 1 xs
rle_aux _ [] = error "empty list"
rle_aux n (x:[]) = (n,x) : []
rle_aux n (x:y:xs) | x == y = rle_aux (n+1) (y:xs)
                   | otherwise = (n,x) : (rle_aux 1 $ y:xs)