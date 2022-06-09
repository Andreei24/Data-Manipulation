
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}
my_atof :: String -> Float
my_atof "" = 0.0
my_atof x = read x

my_atoi :: String -> Int
my_atoi "" = 0
my_atoi x = read x


media h = printf"%.2f" (sum (map my_atof h)/8)

-- Task 1
calculeaza_media :: Row -> Row
calculeaza_media (h:hs) = h : [media (reverse hs)]

compute_average_steps :: Table -> Table
compute_average_steps (h:hs) = ["Name","Average Number of Steps"] : map calculeaza_media hs


-- Task 2

-- Number of people who have achieved their goal:run

my_count :: (Num a1, Foldable t) => [t a2] -> a1
my_count [] = 0
my_count (h:hs) = if null h then 0 + my_count hs else 1 + my_count hs

parse_table :: Table->Int
parse_table (h:hs) = my_count (map (filter (\x-> my_atof x >= 125) . tail) hs)

get_passed_people_num :: Table -> Int
get_passed_people_num tab = parse_table $ compute_average_steps tab


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage tab = fromIntegral (get_passed_people_num tab) / fromIntegral (length tab -1)


-- Average number of daily steps
get_steps :: Table -> [[Value]]
get_steps tab = map tail (tail (compute_average_steps tab))

sum_steps :: Table -> Float
sum_steps tab = sum (map (my_atof . head) (get_steps tab))

get_steps_avg :: Table -> Float
get_steps_avg tab = sum_steps tab / fromIntegral (length tab -1) * 8


-- Task 3
get_avg_per_h :: Table->Int->[String]
get_avg_per_h tab x = [printf "%.2f" ( sum (map (my_atof . (!!x)) (tail tab)) / fromIntegral (length tab-1))]

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h tab = [["H10","H11","H12","H13","H14","H15","H16","H17"], get_avg_per_h tab 1 ++
  get_avg_per_h tab 2 ++
  get_avg_per_h tab 3 ++
  get_avg_per_h tab 4 ++
  get_avg_per_h tab 5 ++
  get_avg_per_h tab 6 ++
  get_avg_per_h tab 7 ++
  get_avg_per_h tab 8]


-- Task 4

get_steps_tab :: [[String]] -> Int -> [Int]
get_steps_tab tab x = map (my_atoi . (!!x)) (tail tab)

count_steps_summary :: [Int]->Int->[String]
count_steps_summary steps x= (if x == 1 then ["VeryActiveMinutes"] else if x == 2 then ["FairlyActiveMinutes"] else ["LightlyActiveMinutes"]) ++
  [show (length (filter (\y-> y >= 0 && y < 50) steps))] ++
  [show (length (filter (\y-> y >= 50 && y < 100) steps))] ++
  [show (length (filter (\y-> y >= 100 && y < 500) steps))]

get_activ_summary :: Table -> Table
get_activ_summary m = [["column","range1","range2","range3"], count_steps_summary ( get_steps_tab physical_activity 3) 1, count_steps_summary ( get_steps_tab physical_activity 4) 2, count_steps_summary ( get_steps_tab physical_activity 5) 3]


-- Task 5
compare_rows :: Row->Row->Ordering
compare_rows x y
    | my_atoi(last x) < my_atoi(last y) = LT
    | my_atoi(last x) > my_atoi(last y) = GT
    | otherwise = if head x < head y then LT else GT


get_ranking :: Table -> Table
get_ranking tab = ["Name","Total Steps"] : sortBy compare_rows (tail (map (take 2) tab))


-- Task 6

sum_first_half :: PrintfType t => [String] -> t
sum_first_half row = printf"%.2f" (sum ( map my_atof (take 4 row)) / 4)

sum_second_half :: PrintfType t => [String] -> t
sum_second_half row = printf"%.2f" (sum (map my_atof (drop 4 row)) / 4)

diff_first_second_half :: Text.Printf.PrintfType t => [String] -> t
diff_first_second_half row = printf "%.2f" (abs (my_atof(sum_first_half row) - my_atof (sum_second_half row)))

get_steps_diff_aux :: Row -> Row
get_steps_diff_aux row = [head row] ++ [sum_first_half (tail row)] ++ [sum_second_half (tail row)] ++ [diff_first_second_half (tail row)]

get_steps_diff_table :: Table -> Table
get_steps_diff_table tab =["Name","Average first 4h","Average last 4h","Difference"] : map get_steps_diff_aux (tail tab)


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f (tail m)


get_sleep_total :: Row -> Row
get_sleep_total r = head r : [printf"%.2f" (sum (map my_atof (tail r)))]



{-
    TASK SET 2
-}

-- Task 1

check_int :: String -> Bool
check_int str = case reads str :: [(Double,String)] of
    [(_,"")] ->True
    _        -> False

get_column_index :: String -> Row -> Int
get_column_index name = (\(Just x)-> x) . elemIndex name


sort_table :: Int -> Table -> Table
sort_table index = sortBy compare_col
    where
        compare_col x y
            | not (check_int (x !! index)) && x !! index == y !! index = EQ
            | not (check_int (x !! index)) && x !! index > y !! index = GT
            | not (check_int (x !! index)) && x !! index < y !! index = LT
            | otherwise = compare_val x y
                where
                    compare_val x y
                        | x !! index == y !! index = compare (head x) (head y)
                        | x !! index == "" = GT
                        | y !! index == "" = LT
                        | otherwise = compare ( my_atof (x !! index)) (my_atof (y !! index))

tsort :: ColumnName -> Table -> Table
tsort column table = head table : sort_table (get_column_index column (head table)) (tail table)

-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = if head t1 == head t2 then t1 ++ tail t2 else t1

-- Task 3
generate_empty_row :: (Eq t, Num t) => t -> [[Char]]
generate_empty_row 0 = []
generate_empty_row len = "" : generate_empty_row (len-1)

check_length :: Int -> Row -> Row
check_length len_t12 row
    | len_t12 > length row = row ++ generate_empty_row (len_t12 - length row)
    | otherwise = row

transpose_tables :: Table -> Table -> Table
transpose_tables t1 t2 = transpose (transpose t1 ++ transpose t2)

hunion :: Table -> Table -> Table
hunion t1 t2 = map (check_length (length (head t1) + length (head t2))) (transpose_tables t1 t2)


-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [["undefined"]]

-- Task 5

cartesian_aux :: (Row -> Row -> Row) -> Row ->Table ->Table
cartesian_aux f x tab = map (f x) (tail tab)

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : foldl(\ x y -> x ++ cartesian_aux new_row_function y t2) [] (tail t1)
-- Task 6

get_projection :: [ColumnName] -> Table -> Table
get_projection _ [] = []
get_projection [] _ = []
get_projection (x:xs) t = transpose t !! get_column_index x (head t) : get_projection xs t

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose (get_projection columns_to_extract t)

-- Task 7

filterTable_aux :: (Value -> Bool) -> Int -> Row ->Row
filterTable_aux condition col_index row
    | condition (row !! col_index) = row
    | otherwise = []

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t :  filter (not . null) (map (filterTable_aux condition (get_column_index key_column (head t))) (tail t))



{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

-- 3.2 & 3.3

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromTable str) = Table []
    eval (AsList str q) = List []
    eval (Sort str q) = Table []
    eval (ValueMap f q) = Table []
    eval (RowMap f s q) = Table []
    eval (VUnion q1 q2) = Table []
    eval (HUnion q1 q2) = Table []
    eval (TableJoin str q1 q2) = Table []
    eval (Cartesian f s q1 q2) = Table []
    eval (Projection s q) = Table []
    eval (Filter c q) = Table []
    eval (Graph op q) = Table []

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval _ _ = const True

instance FEval String where
    feval _ _ = const True
-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5
similarities_query :: Query
similarities_query = FromTable []

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = [["undefined"]]
