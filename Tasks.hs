{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

module Tasks where

import Dataset
import Text.Printf
import Data.List (sort, sortBy, delete)
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


{-
    TASK SET 1
-}

-- Task 1
take_name :: Table -> Table
take_name = foldr (\row acc -> [(head row)]:acc) []

get_grades :: Table -> [Float]
get_grades table = zipWith (+) (qs_calculus table) (take_exam table)

take_questions :: Table -> Table
take_questions x = map tail (map init (tail x))

to_num table = map (map (\num -> if num == "" then 0 else read num)) (take_questions table)

qs_calculus :: Table -> [Float]
qs_calculus table = map (/4) $ map (foldr (\acc num -> acc + num) 0) (to_num table)

take_exam :: Table -> [Float]
take_exam table = map (\num -> read num :: Float) (tail (foldr (\row acc -> (last row):acc) [] table))

compute_exam_grades :: Table -> Table
compute_exam_grades table = zipWith (\name grade-> name ++ [grade]) (take_name table) (full_grades table)
                            where
                                full_grades gr = "Punctaj Exam":(map (printf "%.2f") (get_grades gr))

-- Task 2

-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num table = foldr (\num acc -> if num >= 2.5 then acc + 1 else acc) 0 (get_grades table)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = read $ 
                                    printf "%.2f" $
                                    (read (show (get_passed_students_num table)) :: Float) / fromIntegral (length (tail table))

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = read $ printf "%.2f" $ (foldr (\num acc -> acc + num) 0 (get_grades table)) / fromIntegral (length (tail table))

-- Number of students who gained at least 1.5p from homework:
hw_columns :: Row -> Int -> Float
hw_columns line x = if (line !! x) == "" then 0 else read (line !! x)

get_hw :: Table -> [Float]
get_hw table = map (\line -> (hw_columns line 2) + (hw_columns line 3) + (hw_columns line 4)) (tail table)

get_passed_hw_num :: Table -> Int
get_passed_hw_num table = foldl (\acc num-> if num >= 1.5 then acc + 1 else acc) 0 (get_hw table)

-- Task 3

--Helpers
my_transpose :: [[a]] -> [[a]]
my_transpose ([]:_) = []
my_transpose matrix = (map head matrix) : (my_transpose (map tail matrix))

nr_students table = fromIntegral((length table) - 1)

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = ["Q1","Q2","Q3","Q4","Q5","Q6"]:[compute_avg_qs table]
                            where
                                compute_avg_qs table = foldr (\elm acc -> (show_2_digits elm):acc) [] (avg_qs table)
                                show_2_digits elm = show(read(printf "%.2f" (read(show elm) :: Float)) :: Float)
                                avg_qs table = foldl (\acc elm -> (elm / (nr_students table)):acc) [] (total_points table)
                                total_points table = foldl(\acc list-> (fromIntegral (sum list)):acc) [] (my_transpose (to_num table))

-- Task 4
qs_points :: Table -> [[Integer]]
qs_points table = map (foldl op [0,0,0]) (my_transpose (take_questions table))
                    where
                        op acc num | num == "" ||  (head num) == '0' = [(acc !! 0) + 1, (acc !! 1), (acc !! 2)]
                                   | (head num) == '1' = [(acc !! 0), (acc !! 1) + 1, (acc !! 2)]
                                   | otherwise = [(acc !! 0), (acc !! 1), (acc !! 2) + 1]


get_exam_summary :: Table -> Table
get_exam_summary table = header:(zipWith (++) question_code (map (map show) (qs_points table)))
                        where
                            question_code = [["Q1"], ["Q2"], ["Q3"], ["Q4"], ["Q5"], ["Q6"]]
                            header = ["Q","0","1","2"]
-- Task 5
mark_sort :: Table -> Table
mark_sort = sortBy (\(_:x:_) (_:y:_) -> compare x y)

name_sort :: Table -> Table
name_sort = sortBy (\(x:_) (y:_) -> compare x y)

get_ranking :: Table -> Table
get_ranking table = ["Nume","Punctaj Exam"]:(mark_sort (name_sort (tail (compute_exam_grades table))))

-- Task 6
get_exam_diff :: Table -> [Float]
get_exam_diff table = (map abs) (zipWith (-) (get_grades table) (map (2*) (qs_calculus table)))

create_unsorted_table :: Table -> Table
create_unsorted_table table = last_step table
                    where
                        first_step table = zipWith (++) (tail (take_name table)) (toString_qs table)
                            where
                                toString_qs table = foldr(\mark acc -> [mark]:acc) [] (map (printf "%.2f") (qs_calculus table))
                        second_step table = zipWith (++) (first_step table) (toString_exam table)
                            where
                                toString_exam table = foldr(\mark acc -> [mark]:acc) [] (map (printf "%.2f") (take_exam table))
                        last_step table = zipWith (++) (second_step table) (toString_diff table)
                            where
                                toString_diff table =  foldr(\mark acc -> [mark]:acc) [] (map (printf "%.2f") (get_exam_diff table))

diff_sort :: Table -> Table
diff_sort = sortBy(\x y -> compare (last x) (last y))

get_exam_diff_table :: Table -> Table
get_exam_diff_table table = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"]:(diff_sort (name_sort (create_unsorted_table table)))


-----------------------------------ETAPA 2-----------------------------------

my_split :: Char -> String -> [String]
my_split sep [] = [""]
my_split sep list = foldr(\chr acc -> if chr == sep then "" : acc else (chr:(head acc)):(tail acc)) [[]] list

read_csv :: CSV -> Table
read_csv str = map (my_split ',') $ my_split '\n' str

make_line :: Row -> String
make_line table = tail $ foldr(\row acc -> ',':row ++ acc) "" table

write_csv :: Table -> CSV
write_csv table = tail $ foldr(\row acc-> '\n':(make_line row) ++ acc) [] table

as_list :: String -> Table -> [String]
as_list col_name table | (head (head table)) == col_name = tail (map head table)
                       | otherwise = as_list col_name (map tail table)

find_index_col_name :: String -> Row -> Int
find_index_col_name col_name row = fst (foldl(\acc word -> if (snd acc) == True 
    then acc 
    else if word == col_name 
        then ((fst acc), True) else ((fst acc) + 1, False)) (0,False) row)


isNumber :: String -> Bool
isNumber "" = False
isNumber str = not $ elem False $ map (\letter -> isDigit letter || letter == '.') str

tsort :: String -> Table -> Table
tsort col_name table = (head table):(sortByKey (sortByFirst table))
                        where
                            myIndex = find_index_col_name col_name (head table)
                            sortByKey = sortBy (\row1 row2 -> if isNumber (row1 !! myIndex) == True && isNumber (row2 !! myIndex) == True
                                                              then 
                                                                compare (read (row1 !! myIndex) :: Float) (read (row2 !! myIndex))
                                                              else
                                                                compare (row1 !! myIndex) (row2 !! myIndex))
                            sortByFirst t = sortBy (\row1 row2 -> compare (head row1) (head row2)) (tail t)

vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f) 

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f entries table = entries:(map f (tail table))

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row):[(printf "%.2f" (sum (aux_map row)))]
                    where 
                        aux_map row = map (\x -> if x == "" then 0 else read x :: Float) (tail (tail row))

vunion :: Table -> Table -> Table
vunion t1 t2 | head t1 == head t2 = t1 ++ (tail t2)
             | otherwise = t1

scale_tables :: Table -> Table -> Table
scale_tables t1 t2 | (length t1) == length t2 = t2
                   | otherwise = t2 ++ replicate (length t1 - length t2) (replicate (length (head t2)) "")

hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) t1 (scale_tables t1 t2)


create_title_row :: String -> Row -> Row -> Row
create_title_row key r1 r2 = r1 ++ (foldl op [] r2)
                    where op acc val | ((val == key) || (elem val r1)) = acc
                                     | otherwise = acc ++ [val]

join_from_left :: String -> Table -> Table -> Table
join_from_left key t1 t2 = foldl op [] t1
                        where
                            op acc row | (head row) == key = acc ++ [row]
                                       | (elem (head row) (map head t2)) == False = acc ++ [row]
                                       | otherwise = acc ++ (filter_by row t2)
                            filter_by row t = filter (\val -> if (head row) == (head val) then True else False) t

join_from_right :: String -> Table -> Table -> Table
join_from_right key t1 t2 = foldl op [] t2
                        where
                            key_row_t1 = head $ filter (\val -> if key == (head val) then True else False) t1
                            key_row_t2 = head $ filter (\val -> if key == (head val) then True else False) t2
                            op acc row | (head row) == key = acc
                                       | (elem (head row) (map head t1)) == True = acc
                                       | otherwise = acc ++ [foldl op2 [] key_row_t1]
                                                where
                                                    concat acc2 val= acc2 ++ [row !! (find_index_col_name val key_row_t2)]
                                                    op2 acc2 val | elem val key_row_t2 == True = concat acc2 val
                                                                 | otherwise = acc2 ++ [""]

zipper_opperation :: String -> Table -> Table -> Table
zipper_opperation key t1 t2 = (join_from_left key t1 t2) ++ (join_from_right key t1 t2)

tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = my_transpose $ zipper_opperation key tt1 tt2
                where
                    tt1 = my_transpose t1
                    tt2 = my_transpose t2

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func str t1 t2 = [str] ++ (foldr op [] (tail t1))
                        where
                            op row acc = (map (\row2 -> func row row2) (tail t2)) ++ acc

projection :: [String] -> Table -> Table
projection columns t = my_transpose $ foldl op [] columns
                    where
                        op acc entry | elem entry (map head tt) = acc ++ filter_col entry
                                     | otherwise = acc
                        tt = my_transpose t
                        filter_col entry = filter (\val -> if entry == head val then True else False) tt


----------------------------------- ETAPA 3 ------------------------------------

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value


data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
    show (CSV csv) = show csv
    show (Table table) = write_csv table
    show (List list) = show list

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromCSV str) = Table (read_csv str)
    eval (ToCSV query) = CSV (write_csv qres_t)
                            where Table qres_t = eval query
    eval (AsList colname query) = List (as_list colname qres_t)
                            where Table qres_t = eval query
    eval (Sort colname query) = Table (tsort colname qres_t)
                            where Table qres_t = eval query
    eval (ValueMap op query) = Table (vmap op qres_t)
                            where Table qres_t = eval query
    eval (RowMap op colnames query) = Table (rmap op colnames qres_t)
                            where Table qres_t = eval query
    eval (VUnion query1 query2) = Table (vunion qres_t1 qres_t2)
                            where Table qres_t1 = eval query1
                                  Table qres_t2 = eval query2
    eval (HUnion query1 query2) = Table (hunion qres_t1 qres_t2)
                            where Table qres_t1 = eval query1
                                  Table qres_t2 = eval query2
    eval (TableJoin colname query1 query2) = Table (tjoin colname qres_t1 qres_t2)
                            where Table qres_t1 = eval query1
                                  Table qres_t2 = eval query2
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames qres_t1 qres_t2)
                            where Table qres_t1 = eval query1
                                  Table qres_t2 = eval query2
    eval (Projection colnames query) = Table (projection colnames qres_t)
                            where Table qres_t = eval query
    eval (Filter filt_cond query) = Table ([(head qres_t)] ++ foldl op [] (tail qres_t))
                            where Table qres_t = eval query
                                  op acc row | (feval (head qres_t) filt_cond) row = acc ++ [row] 
                                             | otherwise = acc
    eval (Graph edgeop query) = Table (get_graph edgeop qres_t)
                            where Table qres_t = eval query

get_graph :: (Row -> Row -> Maybe Value) -> Table -> Table
get_graph edge_op table = [["From", "To", "Value"]] ++ (fst $ foldl op ([], 0) my_table)
                    where
                        my_table = tail table
                        op acc row  = ((fst acc) ++ (graph_from_one_row edge_op row (drop (snd acc) my_table)), (snd acc) + 1)

graph_from_one_row :: (Row -> Row -> Maybe Value) -> Row -> Table -> Table
graph_from_one_row edge_op row table = foldr op [] (tail table)
                            where
                                op row1 acc | rows_match edge_op row row1 = ((make_head row row1) ++ [fromJust (edge_op row row1)]):acc 
                                            | otherwise = acc

make_head :: Row -> Row -> Row
make_head row1 row2 | row1 <= row2 = (head row1):[head row2]
                    | otherwise = (head row2):[head row1]

rows_match :: (Row -> Row -> Maybe Value) -> Row -> Row -> Bool
rows_match op row1 row2 | op row1 row2 == Nothing = False
                  | otherwise = True

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

string_to_float :: String -> Float
string_to_float "" = 0
string_to_float str = read str

instance FEval Float where
    feval str (Eq colname ref) = (\row -> string_to_float (row !! (find_index_col_name colname str)) == ref)
    feval str (Gt colname ref) = (\row -> string_to_float (row !! (find_index_col_name colname str)) > ref)
    feval str (Lt colname ref) = (\row -> string_to_float (row !! (find_index_col_name colname str)) < ref)
    feval str (In colname ref) = (\row -> elem (string_to_float (row !! (find_index_col_name colname str))) ref)
    feval str (FNot cond) = (\row -> not ((feval str cond) row))
    feval str (FieldEq colname1 colname2) = (\row -> string_to_float (row !! (find_index_col_name colname1 str)) 
        == string_to_float (row !! (find_index_col_name colname2 str)))

instance FEval String where
    feval str (Eq colname ref) = (\row -> (row !! (find_index_col_name colname str)) == ref)
    feval str (Gt colname ref) = (\row -> (row !! (find_index_col_name colname str)) > ref)
    feval str (Lt colname ref) = (\row -> (row !! (find_index_col_name colname str)) < ref)
    feval str (In colname ref) = (\row -> elem ((row !! (find_index_col_name colname str))) ref)
    feval str (FNot cond) = (\row -> not ((feval str cond) row))
    feval str (FieldEq colname1 colname2) = (\row -> (row !! (find_index_col_name colname1 str)) == 
                                            (row !! (find_index_col_name colname2 str)))

edge_similarities :: Row -> Row -> Maybe Value
edge_similarities row1 row2 | head row1 == "" || head row2 == "" = Nothing
                            | (distance_calculus row1 row2) >= 5 = Just (show (distance_calculus row1 row2))
                            | otherwise = Nothing

distance_calculus :: Row -> Row -> Integer
distance_calculus [] [] = 0
distance_calculus (x:xs) (y:ys) | x == y = 1 + distance_calculus xs ys
                                | otherwise = distance_calculus xs ys

similarities_query :: Query
similarities_query = Sort "Value" $ Graph edge_similarities (FromCSV lecture_grades_csv)


----------------------------------ETAPA 4---------------------------------------



edit_dist :: Eq a => [a] -> [a] -> Int
edit_dist a b = (foldr op [n, n - 1..0] $ zip a [m, m - 1..]) !! 0
                            where
                                (m, n) = (length a, length b)
                                op (s, l) lst = foldr sec_op [l] $ zip3 b lst (tail lst)
                                    where
                                        sec_op (t, i, j) acc = inc:acc
                                            where inc = minimum [i + 1, acc !! 0 + 1, if s == t then j else j + 1]

-- functie care intoarce cel mai apropiat nume de typo
best_fit :: String -> Table -> String
best_fit typo col_t = snd $ head $ sortBy (\(x,_) (y,_) -> compare x y) $ 
                            map (\str -> ((edit_dist typo str), str)) (map head (tail col_t))

get_corrected_col :: Table -> Table -> Table
get_corrected_col typos_t ref_t = map (\typo -> [best_fit typo ref_t]) (map head (tail typos_t))

get_first_part_table :: String -> Table -> Table
get_first_part_table key table = map (\row -> take (find_index_col_name key (head table)) row) (tail table)

get_second_part_table :: String -> Table -> Table
get_second_part_table key table = map (\row -> drop (find_index_col_name key (head table) + 1) row) (tail table)

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname typos ref = write_csv $ [head init_table] ++ 
                                             (hunion (hunion (get_first_part_table colname init_table) 
                                             (get_corrected_col typos_t ref_t)) (get_second_part_table colname init_table))
                            where
                                Table init_table = eval $ FromCSV typos
                                Table typos_t = eval $ Projection [colname] $ FromCSV typos
                                Table ref_t = eval $ Projection [colname] $ FromCSV ref

hw_grade :: Row -> Float
hw_grade row = sum $ map string_to_float (tail row)

lecture_grade :: Row -> Float
lecture_grade row = (2 * (sum (map string_to_float (tail row)))) / (fromIntegral (length (tail row)))

exam_grade :: Row -> Float
exam_grade row = (sum (map string_to_float (init (tail row)))) / 4 + (string_to_float (last row))

final_grade :: String -> String -> String -> String
final_grade from_hw from_lecture from_exam | (string_to_float from_hw) + (string_to_float from_lecture) < 2.5 = "4.00"
                                           | (string_to_float from_exam) < 2.5 = "4.00"
                                           | otherwise = printf "%.2f" ((min ((string_to_float from_hw) + 
                                                                        (string_to_float from_lecture)) 5) + 
                                                                        (string_to_float from_exam))

create_final_student :: Row -> Table -> Table -> Table -> Row
create_final_student stud hw_t exam_t lecture_t = [(head stud)] ++ [from_hw] ++ [from_lecture] ++ [from_exam] ++ [total_points]
                    where
                        total_points = final_grade from_hw from_lecture from_exam
                        from_hw = foldr op "" hw
                            where
                                op row acc | head stud == head row = printf "%.2f" $ hw_grade row
                                           | otherwise = acc
                        from_lecture = foldr op "" lecture
                            where
                                op row acc | head row == head (tail stud) = printf "%.2f" $ lecture_grade row
                                           | otherwise = acc
                        from_exam = foldr op "" exam
                            where
                                op row acc | head row == head stud = printf "%.2f" $ exam_grade row
                                           | otherwise = acc
                        hw = tail hw_t
                        exam = tail exam_t
                        lecture = tail lecture_t

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email_csv hw_csv exam_csv lecture_csv =  write_csv $ 
                                            [["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"]] ++ 
                                            (sort $ foldr (\row acc -> (create_final_student row hw exam lecture):acc) [] (tail email))
        where
            Table email = eval $ FromCSV $ correct_table "Nume" email_csv hw_grades_csv
            Table hw = eval $ FromCSV hw_csv
            Table exam = eval $ FromCSV exam_csv
            Table lecture = eval $ FromCSV lecture_csv