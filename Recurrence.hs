module Recurrence where

-- A module for computing linear recurrence relation.
--
-- Syntax:
--   > findRecurrence [1,1,3,5,8,13,21,34,55,89,144]
--   [1,-1,-1]


import Data.Ratio

findRecurrence l =
  findRecurrence' l [0%1] [1%1] 0
    where
      mmax = div (length l) 2
      findRecurrence' l q1 q2 b
        | null l           = normalize q2
        | head l == 0      = findRecurrence' (tail l) q1 q2 (b+1)
        | length q2 > mmax = [0]
        | otherwise        =
            findRecurrence' (tail $ invert l) q2 (add q1 q2 (head l) b []) 1
      invert (lh:ls) = foldl (\a b -> a ++ [- (sum $ zipWith (*) a b)/lh])
                       [1%1 / lh] (tail $ scanl (\a b -> b:a) [] ls)
      add [] [] _ _ acc = reverse $ dropWhile (0==) acc
      add [] (q2h:q2s) a _ acc = add [] q2s a 0 (q2h/a : acc)
      add (q1h:q1s) [] _ 0 acc = add q1s [] 1 0 (q1h : acc)
      add q1 [] _ b acc = add q1 [] 1 (b-1) (0%1 : acc)
      add (q1h:q1s) (q2h:q2s) a 0 acc = add q1s q2s a 0 (q2h/a + q1h : acc)
      add q1 (q2h:q2s) a b acc = add q1 q2s a (b-1) (q2h/a : acc)
      normalize l@(lh:_) = let c = (\x -> if lh < 0 then -x else x)
                                     (foldl lcm 1 (map denominator l))%1
                             in map (numerator . (c*)) l
