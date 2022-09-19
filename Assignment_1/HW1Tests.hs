{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "run" at the Haskell prompt.  -} 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1
import HW1 (merge_sorted, wins_by_year)

-- P1. merge_sorted tests
-- merge_sorted_test1 = TestCase (assertEqual "merge_sorted_test1"
--                                             []
--                                             (merge_sorted [] []))
merge_sorted_test2 = TestCase (assertEqual "merge_sorted_test2"
                                            [1,2,3]
                                            (merge_sorted [1,2,3] []))
merge_sorted_test3 = TestCase (assertEqual "merge_sorted_test3"
                                            [1,2,3]
                                            (merge_sorted [] [1,2,3]))                                            
merge_sorted_test4 = TestCase (assertEqual "merge_sorted_test4"
                                            [1,2,3,4,5,6]
                                            (merge_sorted [1,2,3] [4,5,6]))
merge_sorted_test5 = TestCase (assertEqual "merge_sorted_test5"
                                            [1,2,3,4,5,6]
                                            (merge_sorted [1,3,5] [2,4,6]))
merge_sorted_test6 = TestCase (assertEqual "merge_sorted_test6"
                                            [1,2,3,4,5,6]
                                            (merge_sorted [2,4,6] [1,3,5]))
merge_sorted_test7 = TestCase (assertEqual "merge_sorted_test7"
                                            "abcdefg"
                                            (merge_sorted "acdf" "beg"))                                
 
-- P2. sum_range tests
sum_range_test1 = TestCase (assertEqual "sum_range_test1" 
                                  0
                                  (sum_range (0,0) [0,1,2,3,4,5,6,7,8,9]) ) 
sum_range_test2 = TestCase (assertEqual "sum_range_test2" 
                                  45
                                  (sum_range (0,9) [0,1,2,3,4,5,6,7,8,9])) 
sum_range_test3 = TestCase (assertEqual "sum_range_test3" 
                                  12
                                  (sum_range (3,5)  [0,1,2,3,4,5,6,7,8,9]) ) 
sum_range_test4 = TestCase (assertEqual "sum_range_test4" 
                                  9
                                  (sum_range (9,9)  [0,1,2,3,4,5,6,7,8,9]) ) 
                        
-- P3. (a) calc_collatz_seq and (b) longest_collatz_seq tests                                  
calc_collatz_seq_test1 = TestCase (assertEqual "calc_collatz_seq_test1" 
                                  [1]
                                  (calc_collatz_seq 0) ) 
calc_collatz_seq_test2 = TestCase (assertEqual "calc_collatz_seq_test2" 
                                  [1]
                                  (calc_collatz_seq 1) ) 
calc_collatz_seq_test3 = TestCase (assertEqual "calc_collatz_seq_test3" 
                                  [12,6,3,10,5,16,8,4,2,1] 
                                  (calc_collatz_seq 12) ) 
calc_collatz_seq_test4 = TestCase (assertEqual "calc_collatz_seq_test4" 
                                  [123,370,185,556,278,139,418,209,628,314,157,472,236,118,59,178,89,268,134,67,202,101,304,152,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]  
                                  (calc_collatz_seq 123) ) 

-- (b) longest_collatz_seq tests
longest_collatz_seq_test1 = TestCase (assertEqual "longest_collatz_seq1" 
                                  [1]
                                  (longest_collatz_seq 0) ) 
longest_collatz_seq_test2 = TestCase (assertEqual "longest_collatz_seq2" 
                                  [1] 
                                  (longest_collatz_seq 1) ) 
longest_collatz_seq_test3 = TestCase (assertEqual "longest_collatz_seq3" 
                                  [19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
                                  (longest_collatz_seq 20) )
longest_collatz_seq_test4 = TestCase (assertEqual "longest_collatz_seq4" 
                                  [97,292,146,73,220,110,55,166,83,250,125,376,188,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
                                  (longest_collatz_seq 100) )

-- P4. (a) game_scores and (b) wins_by_year tests  (one test is sufficient for wins_by_year)                                
wsu_games = [
    (2019, [("NMSU",(58,7)), ("UNCO",(59,17)), ("HOU",(31,24)), ("UCLA",(63,67)), ("UTAH",(13,38)), 
            ("ASU",(34,38)), ("COLO",(41,10)), ("ORE",(35,37)), ("CAL",(20,33)), ("STAN",(49,22)), 
            ("ORST",(54,53)), ("WASH",(13,31)), ("AFA",(21,31))]),
    (2020, [("ORST",(38,28)), ("ORE",(29,43)), ("USC",(13,38)), ("UTAH",(28,45))]),
    (2021, [("USU",(23,26)), ("PORT ST.",(44,24)), ("USC",(14,45)), ("UTAH",(13,24)), ("CAL",(21,6)),
            ("ORST",(31,24)), ("STAN",(34,31)), ("BYU",(19,21)), ("ASU",(34,21)), ("ORE",(24,38)), 
            ("ARIZ",(44,18)), ("WASH",(40,13)), ("CMU",(21,24))] )
            ]
            
game_scores_test1 = TestCase (assertEqual "game_scores_test1" 
                                  []
                                  (sort $ game_scores wsu_games "BERKLEY") ) 
game_scores_test2 = TestCase (assertEqual "game_scores_test2" 
                                  (sort [(63,67)]) 
                                  (sort $ game_scores wsu_games "UCLA") ) 
game_scores_test3 = TestCase (assertEqual "game_scores_test3" 
                                  (sort [(13,38),(28,45),(13,24)])
                                  (sort $ game_scores wsu_games "UTAH") ) 


wins_by_year_test1 = TestCase (assertEqual "wins_by_year_test1" 
                                  []  
                                  (sort $ wins_by_year []) )  
wins_by_year_test2 = TestCase (assertEqual "wins_by_year_test2" 
                                  (sort [(2019,6),(2020,1),(2021,7)])  
                                  (sort $ wins_by_year wsu_games) )                   
-- P5. compress_str tests


-- add the test cases you created to the below list. 
tests = TestList [ --TestLabel "merge_sorted- test1 " merge_sorted_test1,
                   TestLabel "merge_sorted- test2 " merge_sorted_test2,  
                   TestLabel "merge_sorted- test3 " merge_sorted_test3,
                   TestLabel "merge_sorted- test4 " merge_sorted_test4,
                   TestLabel "merge_sorted- test5 " merge_sorted_test5,  
                   TestLabel "merge_sorted- test6 " merge_sorted_test6,
                   TestLabel "merge_sorted- test7 " merge_sorted_test7,

                   TestLabel "sum_range- test1 " sum_range_test1,
                   TestLabel "sum_range- test2 " sum_range_test2,  
                   TestLabel "sum_range- test3 " sum_range_test3,
                   TestLabel "sum_range- test4 " sum_range_test4,

                   TestLabel "calc_collatz_seq- test1 " calc_collatz_seq_test1, 
                   TestLabel "calc_collatz_seq- test2 " calc_collatz_seq_test2, 
                   TestLabel "calc_collatz_seq- test3 " calc_collatz_seq_test3,
                   TestLabel "calc_collatz_seq- test4 " calc_collatz_seq_test4,

                   TestLabel "longest_collatz_seq- test1 " longest_collatz_seq_test1, 
                   TestLabel "longest_collatz_seq- test2 " longest_collatz_seq_test2, 
                   TestLabel "longest_collatz_seq- test3 " longest_collatz_seq_test3,
                   TestLabel "longest_collatz_seq- test4 " longest_collatz_seq_test1,

                   TestLabel "game_scores- test1 " game_scores_test1, 
                   TestLabel "game_scores- test2 " game_scores_test2, 
                   TestLabel "game_scores- test3 " game_scores_test3,
                   
                   TestLabel "wins_by_year- test1 " game_scores_test1,
                   TestLabel "wins_by_year- test2 " game_scores_test2
                 ] 
                  
-- shortcut to run the tests
run = runTestTT  tests