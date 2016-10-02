module Recurrence where

-- A module for computing linear recurrence relation.
--
-- Syntax:
--   > findRecurrence [1,1,3,5,8,13,21,34,55,89,144]
--   [1,-1,-1]


import Data.Ratio
import Data.List (dropWhileEnd)

findRecurrence l =
  findRecurrence' l [0%1] [1%1] 0
    where
      mmax = (div (length l) 2)
      findRecurrence' l q1 q2 b
        | null l           = normalize q2
        | head l == 0      = findRecurrence' (tail l) q1 q2 (b+1)
        | length q2 > mmax = [0]
        | otherwise        =
            findRecurrence' (tail $ invert l) q2 
              (dropWhileEnd (0 ==) $
                take (max (length q2) (b + length q1))
                  (zipWith (+)
                    ((map (\x -> x / (head l)) q2) ++ (repeat 0))
                    ((replicate b 0) ++ q1 ++ (repeat 0)))) 1
      invert l = foldl (\a b -> a ++ [- (sum $ zipWith (*) a b)/(head l)])
                       [1%1 / (head l)]
                       (tail $ scanl (\a b -> b:a) [] (tail l))
      normalize l = let c = (\x -> if (head l)<0 then -x else x)
                              (foldl lcm 1 (map denominator l))%1
                      in map (numerator . (c*)) l

-- alternate version where addition is recursive

findRecurrence2 l =
  findRecurrence' l [0%1] [1%1] 0
    where
      mmax = (div (length l) 2)
      findRecurrence' l q1 q2 b
        | null l           = normalize q2
        | head l == 0      = findRecurrence' (tail l) q1 q2 (b+1)
        | length q2 > mmax = [0]
        | otherwise        =
            findRecurrence' (tail $ invert l) q2 (add q1 q2 (head l) b []) 1
      invert l = foldl (\a b -> a ++ [- (sum $ zipWith (*) a b)/(head l)])
                       [1%1 / (head l)]
                       (tail $ scanl (\a b -> b:a) [] (tail l))
      add [] [] _ _ acc = reverse $ dropWhile (0==) acc
      add [] (q2h:q2s) inva _ acc = add [] q2s inva 0 (q2h/inva:acc)
      add (q1h:q1s) [] _ 0 acc = add q1s [] 1 0 (q1h:acc)
      add q1 [] _ b acc = add q1 [] 1 (b-1) (0:acc)
      add (q1h:q1s) (q2h:q2s) inva 0 acc =
        add q1s q2s inva 0 (q2h/inva + q1h : acc)
      add q1 (q2h:q2s) inva b acc =
        add q1 q2s inva (b-1) (q2h/inva : acc)
      normalize l = let c = (\x -> if (head l)<0 then -x else x)
                              (foldl lcm 1 (map denominator l))%1
                      in map (numerator . (c*)) l
