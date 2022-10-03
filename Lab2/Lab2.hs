-- CptS 355 - Lab 2 (Haskell) - Fall 2022
-- Name: Yeesa Kee
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map once" #-}


module Lab2
     where
import GHC.Classes (leWord)
import Data.Time.Format.ISO8601 (yearFormat)


-- 1
{- (a) merge2 -}
-- merge2: takes two lists and return a list with the elements from both lists in
--         alternating order
merge2 :: [a] -> [a] -> [a]
merge2 [] [] = []
merge2 l1 [] = l1
merge2 [] l2 = l2
merge2 (x:xs) (y:ys) = x:y: merge2 xs ys


{- (b) merge2Tail -}
-- merge2Tail: takes two lists and return a list with elements from both lists in
--             alternating order using tail-recursion
merge2Tail :: [a] -> [a] -> [a]
merge2Tail [] [] = []
merge2Tail l1 [] = l1
merge2Tail [] l2 = l2
merge2Tail l1 l2 = merge2Tail_helper l1 l2 []
     where
          -- merge2Tail_helper: takes three lists, takes the elements in the first two lists
          --                    and store it in the third list in alternating order
          merge2Tail_helper :: [a] -> [a] -> [a] -> [a]
          merge2Tail_helper [] [] accum = accum
          merge2Tail_helper l1 [] accum = accum ++ l1
          merge2Tail_helper [] l2 accum = accum ++ l2
          merge2Tail_helper (x:xs) (y:ys) accum = merge2Tail_helper xs ys (accum ++ (x:[y]))



{- (c) mergeN -}
-- mergeN: takes a list of lists and returns a list of all the sublists combined from left to right
--         with their elements in alternating order without recurssion
mergeN :: [[a]] -> [a]
mergeN [[]] = []
mergeN (x:xs) = foldl merge2 x xs

-- 2
{- (a) count -}
-- count: takes a value and a list and returns the number of times that value appears in the list
count :: Eq a => a -> [a] -> Int
count val [] = 0
count val iL = length (filter (isVal val) iL)
     where 
          isVal :: Eq a => a -> a -> Bool
          isVal val1 val2 = val1 == val2



{- (b) histogram  -}

-- eliminateDuplicates: takes a list and returns a list with all duplicates removed
eliminateDuplicates :: (Foldable t, Eq a) => t a -> [a]
eliminateDuplicates iL = foldr eliminate_helper [] iL
     where 
          eliminate_helper :: Eq a => a -> [a] -> [a]
          eliminate_helper val iL | elem val iL = iL
                                  | otherwise = val:iL

-- histogram: takes a list and returns a list of tuples. First value is a value from the list,
--           second value is the number of times that value appeared in the given list.
histogram :: Eq a => [a] -> [(a, Int)]
histogram [] = []
histogram iL = map (\x -> (x, count x iL)) (eliminateDuplicates iL)



-- 3                
{- (a) concatAll -}
-- contcatAll: takes a nested list of strings and returns the concatenation of all
--             strings in all sublists of the input list.
concatAll :: [[String]] -> String
concatAll [[]] = ""
-- map gives a sublist of iL (which is a list of strings) and calls concat_list on it
-- example: ["enrolled"," ","in"," "] -> "enrolled in "
--          ["CptS","-","355"] -> "CptS-355"
--          ...
-- when map finishes going through every sublist in iL, call concat_list again on the
-- resulting list from map
-- example: ["enrolled in ", "CptS-355", ...] -> "enrolled in CptS-355..."
concatAll iL = concat_list (map concat_list iL)
     where 
          concat_list :: [String] -> String
          concat_list iL = foldl (++) "" iL



{- (b) concat2Either -}
-- AnEither value with values AString with String type or AnInt with Int type
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)

-- concat2Either: given a list of sublists with AnEither values, return an AString with all
--                the values from the lists concatenated.
concat2Either :: [[AnEither]] -> AnEither
concat2Either iL = concat_anEitherlist (map concat_anEitherlist iL)
     where
          -- concat_anEitherlist: given a list of AnEither values, return an AString with all the values
          --                      in the given list concatenated.
          concat_anEitherlist :: Foldable t => t AnEither -> AnEither
          concat_anEitherlist iL = foldl anEither_concat (AString "") iL
               where
                    -- anEither_concat: given two AnEither values, return a AnEither value of the two concatenated
                    anEither_concat :: AnEither -> AnEither -> AnEither
                    anEither_concat (AString x) (AString y) = AString (x ++ y)
                    anEither_concat (AString x) (AnInt y) = AString (x ++ show y)
                    anEither_concat (AnInt x) (AString y) = AString (show x ++ y)
                    anEither_concat (AnInt x) (AnInt y) = AString (show x ++ show y)

-- 4      
{-  concat2Str -}

concat2Str :: [[AnEither]] -> String
concat2Str iL = foldl (++) "" (map concat_string iL)
     where
          -- concat_anEitherlist: given a list of AnEither values, return an AString with all the values
          --                      in the given list concatenated.
          concat_string :: [AnEither] -> String
          concat_string iL = foldl anEither_concat "" iL
               where
                    -- anEither_concat: given two AnEither values, return a AnEither value of the two concatenated
                    anEither_concat :: String -> AnEither -> String
                    anEither_concat x (AString y) = x++y
                    anEither_concat x (AnInt y) = x ++ show y



data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}



-- 6
{- printInfix -}



--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)






