--https://wiki.haskell.org/99_questions/31_to_41
--31: find if number is prime
isPrime :: Int -> Bool
isPrime n = isPrime_aux n (n-1)
           where
               isPrime_aux 0 _ = True
               isPrime_aux 1 _ = True
               isPrime_aux n 1 = True
               isPrime_aux n c | (mod n c) == 0 = False
                               | otherwise = isPrime_aux n (c-1)
                        
--32: compute GCD
mygcd :: Int -> Int -> Int
mygcd a b | a == b = a
          | a < b  = mygcd a (b-a)
          | a > b  = mygcd (a-b) b

--33: find if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b | (mygcd a b) == 1 = True
            | otherwise = False

--34: totient function
totient :: Int -> Int
totient n = totient_aux n (n-1) 0
           where 
                totient_aux _ 0 c = c
                totient_aux n m c | (coprime n m) = totient_aux n (m-1) (c+1)
                                  | otherwise = totient_aux n (m-1) c

--35: compute the list of prime factors of a number in ascending order
primes :: [Int]
primes = 1:2:(primes_aux 3)
        where
            primes_aux n | isPrime n = n : (primes_aux $ n+1)
                         | otherwise = primes_aux $ n+1

primesUntil n = takeWhile (<= n) primes
primesBefore n = takeWhile (< n) primes

primeFactors :: Int -> [Int]
primeFactors n = primeFactors_aux n (tail primes)
                where
                    primeFactors_aux n p@(x:xs) | x > n = []
                                                | (mod n x) == 0 = x : (primeFactors_aux (div n x) p)
                                                | otherwise = (primeFactors_aux n xs)

--36: compress the previous list using couples
rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle xs = rle_aux (tail xs) (head xs) 1
        where rle_aux [] current count = (current,count):[]
              rle_aux (x:xs) current count | x == current = rle_aux xs current (count+1)
                                           | otherwise = (current,count) : (rle_aux xs x 1)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult n = rle $ primeFactors n

--37: improved totient computation
totientImproved :: Int -> Int
totientImproved n = round $ foldl (*) 1 $ map phi $ map coupleFromIntegral $ primeFactorsMult n
                   where 
                       phi (a,b) = (a-1)*(a**(b-1))
                       coupleFromIntegral (a,b) = (fromIntegral a, fromIntegral b)

--39: primes within range
primesR :: Int -> Int -> [Int]
primesR a z = filter (>= a) (primesUntil z)

--40: find the goldbach primes of the input even number
goldbach :: Int -> (Int, Int)
goldbach n | odd n = error "input must be even"
           | n <= 2 = error "input must be > 2"
           | otherwise = head $ [(a,b) | a <- (primesBefore n),
                                         b <- (primesBefore n),
                                         a+b==n ]

--41: find the goldbach primes for a range
goldbachR :: Int -> Int -> [(Int,Int)]
goldbachR a z = map goldbach $ filter even $ filter (> 2) [a..z]