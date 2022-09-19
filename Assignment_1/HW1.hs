-- CptS 355 - Fall 2022 -- Homework1 - Haskell
-- Name: Yeesa Kee
-- Collaborators: ---
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use uncurry" #-}

module HW1
     where

-- P1 - merge_sorted 10%
-- merge_sorted: takes two sorted lists and returns a sorted merged list
merge_sorted :: Ord a => [a] -> [a] -> [a]
merge_sorted [] [] = []    -- base case: if both lists are empty return empty list
merge_sorted [] y = y      -- base case: if first list is empty return second list y
merge_sorted x [] = x      -- base case: if second list is empty return first list x
merge_sorted (x:xs) (y:ys) | x <= y = x: merge_sorted xs (y:ys)       -- if x is less than or equal to y then concat x
                           | otherwise = y:merge_sorted (x:xs) ys     -- otherwise concat y

-- P2  sum_range  15%
-- sum_range: takes a tuple representing indices, a list of numbers, and return the sum of the
--            numbers in between the given indices. (Inclusive of the beginning and end indicies)
sum_range :: (Ord a, Num p, Num a) => (a, a) -> [p] -> p
sum_range (a,b) [] = 0   -- base case: if list is empty return 0
sum_range tp iL = sum_range_helper tp iL 0
     where
          sum_range_helper (a,b) (x:xs) index
                -- if current index is in the given interval add x at index to sum
               | (index >= a && index < b) = x + sum_range_helper (a,b) (xs) (index+1)
               | (index == b) = x    -- if index is the last interval then just return x
               | otherwise = sum_range_helper (a,b) (xs) (index+1)   -- otherwise call function again with index + 1

-- P3  (a) calc_collatz_seq ; 10%
-- calc_collatz_seq: takes a numer and returns a list of the collatz sequence of the given number
calc_collatz_seq :: Integral a => a -> [a]
calc_collatz_seq 0 = [1]    -- base case: if num is 0 return [1]
calc_collatz_seq 1 = [1]    -- base case: if num is 1 return [1]
calc_collatz_seq num
     -- if num is even then concat num and call function with num/2
     | (num `mod` 2 == 0) = num:calc_collatz_seq (num `div` 2)
     -- otherwise concat num and call function with 3 * num + 1
     | otherwise = num:calc_collatz_seq (3 * num + 1)

-- P3  (b) longest_collatz_seq ; 15%
-- longest_collatz_seq: takes an integer and returns a list of integers
longest_collatz_seq :: Integral a => a -> [a]
longest_collatz_seq 0 = [1]    -- base case: if num is 0 return [1]
longest_collatz_seq 1 = [1]    -- base case: if num is 1 return [1]
longest_collatz_seq num =
     longest_collatz_seq_helper (calc_collatz_seq num) (calc_collatz_seq (num-1)) num (num-1)
     where
          -- takes two lists, xL and yL, and compares the length of the lists
          -- (xL stores the longest list so far and yL stores the new list to compare)
          -- takes two integers, xnum and ynum, represents the collatz sequence of xL and yL
          -- returns the list that has the longest collatz sequence
          longest_collatz_seq_helper :: Integral a => [a] -> [a] -> a -> a -> [a]
          longest_collatz_seq_helper [] [] xnum ynum = []    -- base case: if both lists are empty return empty list
          longest_collatz_seq_helper xL [] xnum ynum = xL    -- base case: if second list is empty return first list xL
          longest_collatz_seq_helper [] yL xnum ynum = yL    -- base case: if first list is empty return second list yL
          longest_collatz_seq_helper xL yL xnum ynum
               | length(xL) >= length(yL) =
                    if ynum <= 0 then xL    -- no more numbers to check
                    -- call function again, with xL as the longest list
                    else longest_collatz_seq_helper xL (calc_collatz_seq (ynum-1)) xnum (ynum-1)
               | otherwise =
                    if ynum <= 0 then yL    -- no more numbers to check
                     -- call function again, with yL as the longest list
                    else longest_collatz_seq_helper yL (calc_collatz_seq (ynum - 1)) ynum (ynum-1)

-- P4  (a) game_scores ; 15%
-- game_scores: takes a list of tuples and opponent school's name.
--              tuple format: (year, [school, (tuple of score)])
--              (tuple of score) format: (school's score, opponent school's score)
--              example list: wsu_games = [(2019, [("NMSU",(58,7)), ("UNCO",(59,17))]
--              returns all the scores played against the given school
game_scores :: Eq t => [(a1, [(t, a2)])] -> t -> [a2]
game_scores [] target_school = []    -- base case: if list is empty return empty list
game_scores (x:xs) target_school =
     -- call game_scores_by_year and concat game_scores of the rest of the list
     game_scores_by_year x target_school ++ game_scores xs target_school
     where
          -- game_scores_by_year: takes a tuple and the opponent school's name
          -- tuple format: year, and tuple (x:xs)
          -- first element of x (fst(x)) is name of school, second element of x (snd(x)) is score of game
          -- returns the scores played against the given school
          game_scores_by_year :: Eq t => (a1, [(t, a2)]) -> t -> [a2]
          game_scores_by_year (a,[]) target_school = []
          game_scores_by_year (year, (x:xs)) target_school
               -- if fst(x) is the target school, concat snd(x) and call function again with xs
               | (fst(x) == target_school) = snd(x):game_scores_by_year (year, xs) target_school
               | otherwise = game_scores_by_year (year, xs) target_school -- otherwise call function again with xs

-- P4  (b) wins_by_year ; 10%
-- wins_by_year: takes a list of tuples
--               tuple format: (year, [school, (tuple of score)])
--               (tuple of score) format: (school's score, opponent school's score)
--               example list: wsu_games = [(2019, [("NMSU",(58,7)), ("UNCO",(59,17))]
--               returns a list of tuples in the format: [(year, # wins)]
wins_by_year :: (Num b, Ord a1) => [(a2, [(a3, (a1, a1))])] -> [(a2, b)]
wins_by_year [] = []     -- base case: if list is empty return empty list
wins_by_year (x:xs) = (fst(x), wins_by_year_helper x) : wins_by_year xs
     where
          -- wins_by_year_helper: takes a tuple
          -- tuple format: year, and tuple (x:xs)
          -- first element of x (fst(x)) is name of school, second element of x (snd(x)) is score of game
          -- returns the scores played against the given school

          -- ====== how to write wins_by_year_helper ::========
          wins_by_year_helper (a,[]) = 0
          wins_by_year_helper (year, (x:xs))
               -- get school's score fst(snd(x)) (snd(x) stores the (school's score, oppoenent's score))
               -- fst(snd(x)) gives school's score snd(snd(x)) gives opponent's score
               | (fst(snd(x)) > snd(snd(x))) = 1 + wins_by_year_helper (year, xs)
               | otherwise = wins_by_year_helper (year, xs)


-- P5  compress_str ; 15% 



-- Assignment rules ; 4%
-- Your own tests ; 6%



