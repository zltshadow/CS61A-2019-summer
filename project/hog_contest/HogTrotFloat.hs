{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (assocs, empty, insertWith)
import Data.Monoid (Sum(..), getSum)
import Data.Tuple (swap)

collect_dice :: [Word] -> [(Float, Word)]
collect_dice xs = go <$> M.assocs m
  where
  -- The map that indicates a number in list and the time of its occurrence.
  m :: Map Word Word
  m = foldl' (flip (flip (M.insertWith (+)) 1)) M.empty xs

  len :: Int
  len = length xs

  -- From a pair (number, times of occurrence) to (proportion of occurrence, number).
  -- We put number in the second value here because 2-tuple is a functor, so we can apply
  -- fmap to the tuple in other functions.
  go :: (Word, Word) -> (Float, Word)
  go (k, v) = (fromIntegral v / fromIntegral len, k)

-- The plain result of a dice roll.
plain_dice :: [Word]
plain_dice = [1, 2, 3, 4, 5, 6]

-- All probabilities of one-dice roll.
dice_result_1 :: [(Float, Word)]
dice_result_1 = {- collect_dice plain_dice -}
  [(1 / 6,1),(1 / 6,2),(1 / 6,3),(1 / 6,4),(1 / 6,5),(1 / 6,6)]

-- All probabilities of two-dice roll.
dice_result_2 :: [(Float, Word)]
dice_result_2 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  pure $ if 1 `elem` [x1, x2] then 1 else x1 + x2 -}
  [(11 / 36,1),(1 / 36,4),(1 / 18,5),(1 / 12,6),(1 / 9,7),(5 / 36,8),(1 / 9,9),(1 / 12,10),(1 / 18,11),(1 / 36,12)]

-- All probabilities of three-dice roll.
dice_result_3 :: [(Float, Word)]
dice_result_3 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3] then 1 else x1 + x2 + x3 -}
  [(91 / 216,1),(1 / 216,6),(1 / 72,7),(1 / 36,8),(5 / 108,9),(5 / 72,10),(1 / 12,11),(19 / 216,12),(1 / 12,13),(5 / 72,14),(5 / 108,15),(1 / 36,16),(1 / 72,17),(1 / 216,18)]

-- All probabilities of four-dice roll.
dice_result_4 :: [(Float, Word)]
dice_result_4 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4] then 1 else x1 + x2 + x3 + x4 -}
  [(671 / 1296,1),(1 / 1296,8),(1 / 324,9),(5 / 648,10),(5 / 324,11),(35 / 1296,12),(13 / 324,13),(17 / 324,14),(5 / 81,15),(85 / 1296,16),(5 / 81,17),(17 / 324,18),(13 / 324,19),(35 / 1296,20),(5 / 324,21),(5 / 648,22),(1 / 324,23),(1 / 1296,24)]

-- All probabilities of five-dice roll.
dice_result_5 :: [(Float, Word)]
dice_result_5 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5] then 1 else x1 + x2 + x3 + x4 + x5 -}
  [(4651 / 7776,1),(1 / 7776,10),(5 / 7776,11),(5 / 2592,12),(35 / 7776,13),(35 / 3888,14),(121 / 7776,15),(185 / 7776,16),(85 / 2592,17),(10 / 243,18),(365 / 7776,19),(127 / 2592,20),(365 / 7776,21),(10 / 243,22),(85 / 2592,23),(185 / 7776,24),(121 / 7776,25),(35 / 3888,26),(35 / 7776,27),(5 / 2592,28),(5 / 7776,29),(1 / 7776,30)]

-- All probabilities of six-dice roll.
dice_result_6 :: [(Float, Word)]
dice_result_6 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  x6 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5, x6] then 1 else x1 + x2 + x3 + x4 + x5 + x6 -}
  [(31031 / 46656,1),(1 / 46656,12),(1 / 7776,13),(7 / 15552,14),(7 / 5832,15),(7 / 2592,16),(41 / 7776,17),(71 / 7776,18),(37 / 2592,19),(317 / 15552,20),(623 / 23328,21),(251 / 7776,22),(281 / 7776,23),(1751 / 46656,24),(281 / 7776,25),(251 / 7776,26),(623 / 23328,27),(317 / 15552,28),(37 / 2592,29),(71 / 7776,30),(41 / 7776,31),(7 / 2592,32),(7 / 5832,33),(7 / 15552,34),(1 / 7776,35),(1 / 46656,36)]

-- All probabilities of seven-dice roll.
dice_result_7 :: [(Float, Word)]
dice_result_7 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  x6 <- plain_dice
  x7 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5, x6, x7] then 1 else x1 + x2 + x3 + x4 + x5 + x6 + x7 -}
  [(201811 / 279936,1),(1 / 279936,14),(7 / 279936,15),(7 / 69984,16),(7 / 23328,17),(35 / 46656,18),(455 / 279936,19),(875 / 279936,20),(95 / 17496,21),(805 / 93312,22),(3535 / 279936,23),(4795 / 279936,24),(6055 / 279936,25),(595 / 23328,26),(875 / 31104,27),(8135 / 279936,28),(875 / 31104,29),(595 / 23328,30),(6055 / 279936,31),(4795 / 279936,32),(3535 / 279936,33),(805 / 93312,34),(95 / 17496,35),(875 / 279936,36),(455 / 279936,37),(35 / 46656,38),(7 / 23328,39),(7 / 69984,40),(7 / 279936,41),(1 / 279936,42)]

-- All probabilities of eight-dice roll.
dice_result_8 :: [(Float, Word)]
dice_result_8 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  x6 <- plain_dice
  x7 <- plain_dice
  x8 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5, x6, x7, x8] then 1 else x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 -}
  [(1288991 / 1679616,1),(1 / 1679616,16),(1 / 209952,17),(1 / 46656,18),(5 / 69984,19),(55 / 279936,20),(49 / 104976,21),(413 / 419904,22),(131 / 69984,23),(1825 / 559872,24),(275 / 52488,25),(365 / 46656,26),(1145 / 104976,27),(665 / 46656,28),(1225 / 69984,29),(2125 / 104976,30),(515 / 23328,31),(38165 / 1679616,32),(515 / 23328,33),(2125 / 104976,34),(1225 / 69984,35),(665 / 46656,36),(1145 / 104976,37),(365 / 46656,38),(275 / 52488,39),(1825 / 559872,40),(131 / 69984,41),(413 / 419904,42),(49 / 104976,43),(55 / 279936,44),(5 / 69984,45),(1 / 46656,46),(1 / 209952,47),(1 / 1679616,48)]

-- All probabilities of nine-dice roll.
dice_result_9 :: [(Float, Word)]
dice_result_9 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  x6 <- plain_dice
  x7 <- plain_dice
  x8 <- plain_dice
  x9 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5, x6, x7, x8, x9] then 1 else x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 -}
  [(8124571 / 10077696,1),(1 / 10077696,18),(1 / 1119744,19),(5 / 1119744,20),(55 / 3359232,21),(55 / 1119744,22),(71 / 559872,23),(487 / 1679616,24),(335 / 559872,25),(1265 / 1119744,26),(19855 / 10077696,27),(1193 / 373248,28),(5431 / 1119744,29),(23225 / 3359232,30),(325 / 34992,31),(275 / 23328,32),(3965 / 279936,33),(18065 / 1119744,34),(19525 / 1119744,35),(180325 / 10077696,36),(19525 / 1119744,37),(18065 / 1119744,38),(3965 / 279936,39),(275 / 23328,40),(325 / 34992,41),(23225 / 3359232,42),(5431 / 1119744,43),(1193 / 373248,44),(19855 / 10077696,45),(1265 / 1119744,46),(335 / 559872,47),(487 / 1679616,48),(71 / 559872,49),(55 / 1119744,50),(55 / 3359232,51),(5 / 1119744,52),(1 / 1119744,53),(1 / 10077696,54)]

-- All probabilities of ten-dice roll.
dice_result_10 :: [(Float, Word)]
dice_result_10 = {- collect_dice $ do
  x1 <- plain_dice
  x2 <- plain_dice
  x3 <- plain_dice
  x4 <- plain_dice
  x5 <- plain_dice
  x6 <- plain_dice
  x7 <- plain_dice
  x8 <- plain_dice
  x9 <- plain_dice
  x10 <- plain_dice
  pure $ if 1 `elem` [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] then 1 else x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 -}
  [(50700551 / 60466176,1),(1 / 60466176,20),(5 / 30233088,21),(55 / 60466176,22),(55 / 15116544,23),(715 / 60466176,24),(83 / 2519424,25),(545 / 6718464,26),(605 / 3359232,27),(3685 / 10077696,28),(20735 / 30233088,29),(72403 / 60466176,30),(14795 / 7558272,31),(182005 / 60466176,32),(66055 / 15116544,33),(121055 / 20155392,34),(78949 / 10077696,35),(24475 / 2519424,36),(38525 / 3359232,37),(780175 / 60466176,38),(209275 / 15116544,39),(856945 / 60466176,40),(209275 / 15116544,41),(780175 / 60466176,42),(38525 / 3359232,43),(24475 / 2519424,44),(78949 / 10077696,45),(121055 / 20155392,46),(66055 / 15116544,47),(182005 / 60466176,48),(14795 / 7558272,49),(72403 / 60466176,50),(20735 / 30233088,51),(3685 / 10077696,52),(605 / 3359232,53),(545 / 6718464,54),(83 / 2519424,55),(715 / 60466176,56),(55 / 15116544,57),(55 / 60466176,58),(5 / 30233088,59),(1 / 60466176,60)]

-- Get the minimal digit of a natural number.
min_digit :: Word -> Word
min_digit n =
  if n == 0
    then 0
    else min_digit_inner (n `quot` 10) (n `rem` 10)
  where
  min_digit_inner :: Word -> Word -> Word
  min_digit_inner m min_till_now =
    if m == 0
      then min_till_now
      else min_digit_inner all_but_last (min last_digit min_till_now)
    where
    (all_but_last, last_digit) = m `quotRem` 10

-- Get the leftmost digit of a natural number.
leftmost_digit :: Word -> Word
leftmost_digit = until (< 10) (`quot` 10)

-- Apply swap if necessary according to the rule of Swine Swap,
-- and return as it is if unnecessary.
apply_swap :: (Word, Word) -> (Word, Word)
apply_swap a@(score, oppon) = if should_swap then swap a else a
  where
  score_leftmost = leftmost_digit score
  oppon_rightmost = oppon `rem` 10
  should_swap = score_leftmost == oppon_rightmost

-- Sum up all possibilities.
fold_probs :: [(Float, Float)] -> Float
fold_probs = getSum . fold . fmap (Sum . uncurry (*))

-- Find the maximum value and its index in a list.
maximum_index_and_value :: Ord a => [a] -> (Word, a)
maximum_index_and_value [] = undefined
maximum_index_and_value (x : xs) = maximum_index_and_value_inner xs x 1 0

maximum_index_and_value_inner :: Ord a => [a] -> a -> Word -> Word -> (Word, a)
maximum_index_and_value_inner [] ever_max _ ever_max_index = (ever_max_index, ever_max)
maximum_index_and_value_inner (x : xs) ever_max this_index ever_max_index =
  if x > ever_max
    then maximum_index_and_value_inner xs x (this_index + 1) this_index
    else maximum_index_and_value_inner xs ever_max (this_index + 1) ever_max_index

win_prob :: Word -> Bool -> (Word, Word) -> (Word, Float)
win_prob t i (s, o) = (f0 <$> (zip [(0 :: Int)..] (repeat (zip [(0 :: Int)..] (repeat (zip [(0 :: Int)..] (repeat [(0 :: Int)..]))))))) !! (fromEnum t) !! (fromEnum i) !! (fromEnum s) !! (fromEnum o)
  where
  f0 :: (Int, [(Int, [(Int, [Int])])]) -> [[[(Word, Float)]]]
  f0 (dim0, xs) = fmap (f1 dim0) xs

  f1 :: Int -> (Int, [(Int, [Int])]) -> [[(Word, Float)]]
  f1 dim0 (dim1, xs) = fmap (f2 dim0 dim1) xs

  f2 :: Int -> Int -> (Int, [Int]) -> [(Word, Float)]
  f2 dim0 dim1 (dim2, xs) = fmap (f3 dim0 dim1 dim2) xs

  f3 :: Int -> Int -> Int -> Int -> (Word, Float)
  f3 dim0 dim1 dim2 dim3 = plain_win_prob (toEnum dim0) (toEnum dim1) (toEnum dim2) (toEnum dim3)

  plain_win_prob :: Word -> Bool -> Word -> Word -> (Word, Float)
  plain_win_prob turn is_extra_turn score oppon
    | score >= 100 = (0, 1.0)
    | oppon >= 100 = (0, 0.0)
    | otherwise = maximum_index_and_value $
        [ throw_0_prob
        , throw_1_prob
        , throw_2_prob
        , throw_3_prob
        , throw_4_prob
        , throw_5_prob
        , throw_6_prob
        , throw_7_prob
        , throw_8_prob
        , throw_9_prob
        , throw_10_prob
        ]
      where
      plain_calc_prob :: Word -> Float
      plain_calc_prob gain = (1 -) $ snd $ win_prob (turn + 1) False $ swap $ apply_swap (score + gain, oppon)

      calc_prob :: Word -> Word -> Float
      calc_prob k gain =
        if not is_extra_turn && turn `rem` 5 == k
          then snd $ win_prob (turn + 1) True $ apply_swap (score + gain, oppon)
          else (1 -) $ snd $ win_prob (turn + 1) False $ swap $ apply_swap (score + gain, oppon)

      throw_0_prob :: Float
      throw_0_prob = plain_calc_prob (10 - min_digit oppon)

      throw_n_prob :: Word -> [(Float, Word)] -> Float
      throw_n_prob k = fold_probs . (parMap rseq) (fmap (calc_prob k))

      throw_1_prob = throw_n_prob 1 dice_result_1
      throw_2_prob = throw_n_prob 2 dice_result_2
      throw_3_prob = throw_n_prob 3 dice_result_3
      throw_4_prob = throw_n_prob 4 dice_result_4
      throw_5_prob = throw_n_prob 5 dice_result_5
      throw_6_prob = throw_n_prob 6 dice_result_6
      throw_7_prob = throw_n_prob 7 dice_result_7
      throw_8_prob = throw_n_prob 8 dice_result_8
      throw_9_prob = throw_n_prob 9 dice_result_9
      throw_10_prob = throw_n_prob 10 dice_result_10

get_win_prob :: Word -> Word -> Word
get_win_prob score oppon = fst $ win_prob 0 False (score, oppon)

-- main = putStrLn "Hello, World!"
main :: IO ()
main = print $ do
  x <- [0..99]
  pure $ do
    y <- [0..99]
    pure $ get_win_prob x y
