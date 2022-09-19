-- CptS 355 - Lab 1 (Haskell) - Fall 2022
-- Name: Yeesa Kee
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use map" #-}


module Lab1
     where

copyList [] = []
copyList (x:xs) = x:(copyList xs)

-- 1.insert 

insert n item [] | n == 0 = [item]
                 | otherwise = []
insert n item (x:xs) | (n==0) = item:x:xs
                     | otherwise = x:(insert (n-1) item xs)


-- 2. insertEvery
insertEveryHelper n index item [] = []
insertEveryHelper n index item (x:xs) | (index `mod` n == 0) = x:item:(insertEveryHelper n (index + 1) item xs)
                                      | otherwise = x:(insertEveryHelper n (index + 1) item xs)

insertEvery n item [] = []
insertEvery n item (x:xs) = insertEveryHelper n 1 item (x:xs)


-- 3. getSales
getSales day [] = 0
getSales day (x:xs) | (fst(x) == day) = snd(x) + getSales day xs
                    | otherwise = getSales day xs
                                                  
-- 4. sumSales
sumSales store day [] = 0
sumSales store day ((a, b):xs) | (a == store) = getSales day b + sumSales store day xs
                          | otherwise = sumSales store day xs

-- 5. split

split c [] = []
split c iL = splitHelper c iL []
     where
          splitHelper c [] [] = []
          splitHelper c [] buff = [reverse buff]
          splitHelper c (x:xs) buff | (x == c) = (reverse buff):splitHelper c xs []
                                    | otherwise = splitHelper c xs (x:buff)

-- 6. nSplit

-- unfortunately isn't working :(
-- nSplit :: [a] -> t -> [a] -> [[a]]

-- nSplit c n [] = [[]]
-- nSplit c n iL = nSplitHelper c n iL []
--      where
--           nSplitHelper:: [a] -> t -> [a] -> [a] -> [[a]]
--           nSplitHelper c n [] [] = [[]]
--           nSplitHelper c n [] buff = [reverse buff]
--           nSplitHelper c n (x:xs) buff | (n < 0) = (reverse buff:(x:xs))
--                                        | (x == c) = (reverse buff):nSplitHelper c (n-1) xs []
--                                        | otherwise = nSplitHelper c n xs (x:buff)

