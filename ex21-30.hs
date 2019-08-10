--https://wiki.haskell.org/99_questions/21_to_28

import System.Random
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)

--21: insert element in a given position in a list
insertAt :: a -> [a] -> Int -> [a]
{-insertAt y xs n = insertAt_aux y xs n []
    where         
        insertAt_aux _ _ 0 _ = error "position must be >=1"
        insertAt_aux y xs 1 acc = (reverse acc) ++ (y:xs)
        insertAt_aux y (x:xs) n acc = insertAt_aux y xs (n-1) (x:acc)
-}
insertAt y xs n = (take (n-1) xs) ++ ( y : (drop (n-1) xs))

--22: create a list with integers in a range
range :: Integral a => a -> a -> [a]
--range a z = [a..z]
range a z | a == z = [z]
range a z = a : range (a+1) z
