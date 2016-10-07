module Permutations where

-- A module for quickly indexing permutations or building them
-- from their index.
--
-- Syntax:
--   > permIndex [5,4,2,1,8,7,0,6,3]
--   223435
--   > permNth 9 223435
--   [5,4,2,1,8,7,0,6,3]

factorials = scanl (*) 1 [1..]

factorial :: Int -> Integer
factorial n = factorials!!n


permIndex :: [Int] -> Integer
permIndex l = permIndex' ((length l)-1) l 0
    where permIndex' :: Int -> [Int] -> Integer -> Integer
          permIndex' 0 _ s = s
          permIndex' n (h:t) s = permIndex' (pred n)
                                            [if x<h then x else pred x | x <- t]
                                            (s + (toInteger h)*factorial n)


permNth :: Int -> Integer -> [Int]
permNth k n = permNth' n [] [0..(k-1)] (k-1)
    where permNth' :: Integer -> [Int] -> [Int] -> Int -> [Int]
          permNth' 0 p (rh:_) _ = reverse (rh:p)
          permNth' n p r f = permNth' (mod n fact) (qh:p) (q ++ qt) (pred f)
              where fact = factorial f
                    (q,qh:qt) = splitAt (fromIntegral $ div n fact) r
