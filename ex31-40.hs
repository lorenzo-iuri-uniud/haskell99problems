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
                        
