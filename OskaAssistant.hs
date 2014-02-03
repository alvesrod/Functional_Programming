-- Developed by: Rodrigo Alves and Kevin Miller

{----------------------------------------------------------------
---------------------------- PURPOSE ----------------------------

This application assists you when playing the "Oska" board game.
It shows you what move you should do next.

------------------------------------------------------------------
-----------------------------------------------------------------}



{----------------------------------------------------------------
---------------------- MAIN PUBLIC FUNCTIONS --------------------
-----------------------------------------------------------------}

-- The main function of the application. Calculates the next best move for Oska.
-- @param board: A list of 2n-3 elements representing the current state of the board.
-- where n is how many pieces a player starts with.   
--  eg: ----------------- 
--      | w | w | w |   |
--      -----------------
--        |   | w |   |
--        -------------
--          |   |   |
--        -------------
--        | b |   |   |
--      -----------------
--      |   | b | b | b | represents the board ["www-","-w-","--","b--","-bbb"].
--      -----------------
-- @param side: the side that the program should play (always 'w' or 'b').
-- @param depth: integer that indicates how deep to search for the best move.
-- @return: the next move or the @param board if there's no next move possible.
-- Call example: oska_q1e8 ["wwww","---","--","---","bbbb"] 'w' 2
oska_q1e8 :: [String] -> Char -> Int -> [String]
oska_q1e8 board side depth
        | not (valid_board_q1e8 board) = board
        | depth < 1                    = board -- DEPTH MUST BE AT LEAST 1
        | side == 'w'                  = search_q1e8 board False depth
        | side == 'b'                  = search_q1e8 board True depth
        | otherwise                    = board

-- Calls the oska function (same params), but prints the next move nicely.
-- Call example: oska_print_q1e8 ["www-","-w-","--","b--","-bbb"] 'b' 4
-- Call example 2: oska_print_q1e8 ["--w-","---","--","-b-","----"] 'b' 3
oska_print_q1e8 :: [String] -> Char -> Int -> IO () 
oska_print_q1e8 board side depth = print_q1e8 (oska_q1e8 board side depth)

-- Simulate a game between two players. Display each move from each player
-- Call example: play_simulation_q1e8 ["--w-","---","--","-b-","----"] 'b' 5
-- Call example 2: play_simulation_q1e8 ["wwww","---","--","---","bbbb"] 'w' 6    
play_simulation_q1e8 :: [String] -> Char -> Int -> IO ()    
play_simulation_q1e8 board side depth = do
        putStrLn ("You are player " ++ [side] ++ ". You start playing.")
        print_q1e8 board
        simulate_game_q1e8 board side depth True
        

{----------------------------------------------------------------
----------------------------- SEARCHING -------------------------
-----------------------------------------------------------------}      

-- Given a function that can generate all possible future states,
-- do a minimax.
-- @return: the next best move
search_q1e8 :: [String] -> Bool -> Int -> [String]
search_q1e8 state side depth = nextState
        where (nextState, _) = minimax_q1e8 state side depth True

-- @param state: the current state that will be checked.
-- @param side: the side that is playing now. True if bottom. False if top
-- @param depth: how more deep to search for a good heuristic
-- @param turn: true if it's your turn to play
-- @return: the next best state and its heuristic value
-- Call example: minimax_q1e8 ["www-","-w-","--","b--","-bbb"] True 3 True
minimax_q1e8 :: [String] -> Bool -> Int -> Bool -> ([String], Int)
minimax_q1e8 state side 0 turn = (["Leaf"], heuristic_q1e8 state yourSide turn)
        where yourSide = yourSide_q1e8 side turn
minimax_q1e8 state side depth turn = chosenState
        where next = next_states_q1e8 state side -- get all possible moves
              states = [ getTuple_q1e8 x side depth turn | x <- next]
              chosenState = max_min_q1e8 turn states

-- Helper for the minimax function.
-- @return: a tuple with the param state and its heuristic
getTuple_q1e8 :: [String] -> Bool -> Int -> Bool -> ([String], Int)              
getTuple_q1e8 state side depth turn = (state, h)
        where (_, h) = minimax_q1e8 state (not side) (depth-1) (not turn)

{----------------------------------------------------------------
---------------------- GENERATING NEW STATES --------------------
-----------------------------------------------------------------}   

-- @param state: the board. It's safe to assume this board is in a valid format.
-- @param side: a boolean representing the side that the program should play.
-- True if bottom player (b). False if top player (w).
-- @return: an array with all possible next moves for player "side".
-- @return: the same state if no possible new states
next_states_q1e8 :: [String] -> Bool -> [[String]]
next_states_q1e8 state side
        | null next = [state]
        | otherwise = next
        where next = next_states_helper_q1e8 state side
next_states_helper_q1e8 :: [String] -> Bool -> [[String]]
next_states_helper_q1e8 state side
        | side        = compute_next_states_q1e8 state
        | otherwise = flip_results_top_q1e8 (compute_next_states_q1e8 (flip_top_q1e8 state)) []

-- Given a board, print on the screen all the next moves.
-- This is just a visual helper for the next_states function.
-- Call example: print_next_states_q1e8 ["wwww","---","--","---","bbbb"] False
print_next_states_q1e8 :: [String] -> Bool -> IO ()
print_next_states_q1e8 state side = print_boards_q1e8 (next_states_q1e8 state side)

{----------------------------------------------------------------
----------------------- NEW STATES HELPERS ----------------------
-----------------------------------------------------------------}   

compute_next_states_q1e8 :: [String] -> [[String]]
compute_next_states_q1e8 state = ((compute_next_states_helper_q1e8 state 0 0 []) 
        ++ (flip_results_left_q1e8 (compute_next_states_helper_q1e8 (flip_left_q1e8 state) 0 0 []) []))
        
compute_next_states_helper_q1e8 :: [String] -> Int -> Int -> [[String]] -> [[String]]
compute_next_states_helper_q1e8 state row col states
        | (row >= length state)                 = states
        | (col >= length (state !! row))        = compute_next_states_helper_q1e8 state (row + 1) 0 states
        | (state !! row) !! col == 'b' && not (null single_piece_moved)    = compute_next_states_helper_q1e8 state row (col + 1) (states ++ [single_piece_moved])
        | otherwise                                                        = compute_next_states_helper_q1e8 state row (col + 1) states
        where single_piece_moved = move_single_piece_q1e8 state row col
      
-- Assumes the piece is black (board has been flipped appropriately) and only attemps to move up and to the left          
move_single_piece_q1e8 :: [String] -> Int -> Int -> [String]
move_single_piece_q1e8 state row col
        | row == 0                                              = []  -- At top row of board (check may not be required)
        | (on_top_half_q1e8 state row) &&                             -- At the far left on the bottom-half of the board (check may not be required)
          (next_position_is_q1e8 state row col 'w')             = take_piece_q1e8 state (row - 1) col -- Potentially able to take a piece on the top-half of the board
        | next_position_is_q1e8 state row col 'w'               = take_piece_q1e8 state (row - 1) (col - 1)  -- Potentially able to take a piece on the bottom-half of the board
        | next_position_is_q1e8 state row col '-'               = move_piece_helper_q1e8 state row col
        | otherwise                                             = []

take_piece_q1e8 :: [String] -> Int -> Int -> [String]
take_piece_q1e8 state row col
        | row == 0      = [] -- Trying to take a piece at the end of board
        | (next_position_is_q1e8 state row col '-') &&
          (row == length (head state) - 2) = (take (length front - 1) front) ++ [(set_square_q1e8 (state !! (row - 1)) 'b' col)] ++
                [(set_square_q1e8 (state !! row)           '-' col)] ++
                [(set_square_q1e8 (state !! (row + 1)) '-' (col + 1))] ++ (tail (tail rear))        -- Center of board
        | (next_position_is_q1e8 state row col '-') &&
          (on_top_half_q1e8 state row) = (take (length front - 1) front) ++ [(set_square_q1e8 (state !! (row - 1)) 'b' col)] ++
                  [(set_square_q1e8 (state !! row)           '-' col)] ++ 
                  [(set_square_q1e8 (state !! (row + 1)) '-' col)] ++ (tail (tail rear)) -- Top-half of board
        | next_position_is_q1e8 state row col '-'  = (take (length front - 1) front) ++ [(set_square_q1e8 (state !! (row - 1)) 'b' (col - 1))] ++
                   [(set_square_q1e8 (state !! row) '-' col)] ++
                   [(set_square_q1e8 (state !! (row + 1)) '-' (col + 1))] ++ (tail (tail rear)) -- Bottom-half of board
        | otherwise = []
        where (front, rear) = splitAt row state

on_top_half_q1e8 :: [String] -> Int -> Bool
on_top_half_q1e8 state row
        | row < (length (head state) - 1) = True
        | otherwise  = False
        
next_position_is_q1e8 :: [String] -> Int -> Int -> Char -> Bool
next_position_is_q1e8 state row col piece
        | (on_top_half_q1e8 state row &&
          ((state !! (row - 1)) !! col) == piece) = True                -- Top-half of board
        | (not(on_top_half_q1e8 state row) &&
          (col > 0) &&
          ((state !! (row - 1)) !! (col - 1)) == piece) = True          -- Bottom-half of board
        | otherwise = False
        
move_piece_helper_q1e8 :: [String] -> Int -> Int -> [String]
move_piece_helper_q1e8 state row col
        | row < (length (head state) - 1)        = front ++ [(set_square_q1e8 (state !! (row - 1)) 'b' col)] ++
                  [(set_square_q1e8 (state !! row) '-' col)] ++ (tail (tail rear))    -- Top half of the board
        | otherwise = front ++ [(set_square_q1e8 (state !! (row - 1)) 'b' (col - 1))] ++
                  [(set_square_q1e8 (state !! row) '-' col)] ++ (tail (tail rear))    -- Bottom half of the board
        where (front, rear) = splitAt (row - 1) state
        
set_square_q1e8 :: String -> Char -> Int -> String
set_square_q1e8 row piece pos = front ++ [piece] ++ (tail rear)
        where (front, rear) = splitAt pos row

{----------------------------------------------------------------
---------------------------- HEURISTIC --------------------------
-----------------------------------------------------------------}   

-- @param state: the board. It's safe to assume this board is in a valid format.
-- @param side: a boolean representing your side
-- True if bottom player (b). False if top player (w).
-- @param turn: a boolean. True if it's your turn. False otherwise.
-- @return: A number representing the board evaluation for you.
-- eg: -10 if you lost. +10 if you won. Some number between otherwise.
-- PS: This function does not do any search. The evaluation is local.
-- Call example: heuristic_q1e8 ["wwww","---","--","---","----"] True True -> -10
-- Call example 2: heuristic_q1e8 ["b---","---","--","-w-","bww-"] False True
heuristic_q1e8 :: [String] -> Bool -> Bool -> Int
heuristic_q1e8 state side turn
        | w_count_q1e8 state == 0       = 10 * (negate_q1e8 (not side)) -- No Ws left. Someone won/lost
        | b_count_q1e8 state == 0       = 10 * (negate_q1e8 side)       -- No Bs left. Someone won/lost
        | otherwise                     = distance_heuristic_q1e8 state side
        

-- This is one possible heuristic evaluation. It calculates how far you
-- are from getting all your remaining pieces to the other side of the board
-- and compare it with how far the enemy is from doing the same.
-- @return a value between -10 and 10 based on the distance remaining.
-- Call example: distance_heuristic_q1e8 ["ww--","---","-b","b-w","-bb-"] False True -> 2
distance_heuristic_q1e8 :: [String] -> Bool -> Int
distance_heuristic_q1e8 state side
        | enemyDistance == 0 = -10 -- All enemy pieces are on the other side. Game over.
        | yourDistance == 0  = 10 -- All your pieces are on the other side. You won.
        | otherwise = (9 * comparedDistance) `div` maxDifference -- A value from -9 to 9            
        where maxDifference = (length (head state)) * (length state) - 1
              enemyDistance = distance_to_goal_q1e8 state (not side)
              yourDistance = distance_to_goal_q1e8 state side
              comparedDistance = enemyDistance - yourDistance

{----------------------------------------------------------------
------------------------- HELPER FUNCTIONS ----------------------
-----------------------------------------------------------------}

-- @return 1 if it's your turn. 0 otherwise.
one_if_turn_q1e8 :: Bool -> Int              
one_if_turn_q1e8 True  = 1
one_if_turn_q1e8 False = 0

-- Given a side to play, return the other side
other_side_q1e8 :: Char -> Char     
other_side_q1e8 'w' = 'b'
other_side_q1e8 'b' = 'w'

-- Simulate a game between two players. Display each move from each player
-- Call example: simulate_game_q1e8 ["--w-","---","--","-b-","----"] 'b' 5
simulate_game_q1e8 :: [String] -> Char -> Int -> Bool -> IO ()
simulate_game_q1e8 board side depth turn
        | h == 10               = putStrLn "Game Over. You won!"
        | h == -10              = putStrLn "Game Over. You lost!"
        | otherwise = do
                oska_print_q1e8 board side depth
                simulate_game_q1e8 nextBoard (other_side_q1e8 side) depth (not turn)
        where nextBoard = oska_q1e8 board side depth
              h = heuristic_q1e8 board (bool_side_q1e8 side) turn
       
-- @return the player side in Bool form
bool_side_q1e8 :: Char -> Bool
bool_side_q1e8 'w' = False
bool_side_q1e8 'b' = True

-- @return the minimum number of moves required to move all remaining
-- pieces to the other side of the board.
-- @param side: True if bottom player (b). False if top player (w).
-- Call example: distance_to_goal_q1e8 ["wwww","---","b-","---","-b-b"] True -> 10
distance_to_goal_q1e8 :: [String] -> Bool -> Int
distance_to_goal_q1e8 board side
        | side       =       distance_to_goal_helper_q1e8 board 0 0
        | otherwise  =  distance_to_goal_helper_q1e8 (flip_top_q1e8 board) 0 0
distance_to_goal_helper_q1e8 :: [String] -> Int -> Int -> Int
distance_to_goal_helper_q1e8 [] _ sum = sum
distance_to_goal_helper_q1e8 (x:xs) rowNum sum = 
        distance_to_goal_helper_q1e8 xs (rowNum+1) (sum + rowNum * (b_row_count_q1e8 x))

-- @return the max element of a list if isMax is true.
-- @return the min element of a list if isMax is false.
max_min_q1e8 :: Bool -> [(a, Int)] -> (a, Int)  
max_min_q1e8 _  [x] = x      
max_min_q1e8 isMax (x:xs) = compare_helper_q1e8 xs isMax x    

-- Helper to get the max or min element of a list.
compare_helper_q1e8 :: [(a, Int)] -> Bool -> (a, Int) -> (a, Int)
compare_helper_q1e8 [] _ bestResult = bestResult
compare_helper_q1e8 (x@(xData, xValue):xs) isMax c@(cData, cValue)
        | compare_q1e8 isMax xValue cValue = compare_helper_q1e8 xs isMax x
        | otherwise                        = compare_helper_q1e8 xs isMax c
        
-- Compare val1 and val2. If isMax is true, true if val1 is bigger than val2.
compare_q1e8 :: Bool -> Int -> Int -> Bool
compare_q1e8 isMax val1 val2 = 
        (isMax && (val1 > val2)) || ((not isMax) && (val1 < val2))        
        
-- Given the side that is playing and if it's your turn, return
-- the boolean value that represents your side as a player.
yourSide_q1e8 :: Bool -> Bool -> Bool
yourSide_q1e8 sidePlaying yourTurn -- XNOR
        | yourTurn      = sidePlaying
        | otherwise     = not sidePlaying

-- Given a board, flip it so that the top player becomes the bottom player.
-- That will also swap all the pieces (all b's will become w's).     
-- Call example: flip_top_q1e8 ["w---","-w-","--","b--","-bbb"]
-- -> returns ["-www","w--","--","-b-","b---"]
flip_top_q1e8 :: [String] -> [String]
flip_top_q1e8 board = reverse (swap_pieces_q1e8 board)

-- Vertically flips all the boards in the given result list
flip_results_top_q1e8 :: [[String]] -> [[String]] -> [[String]]
flip_results_top_q1e8 results flipped_results
        | null results        = flipped_results
        | otherwise                = flip_results_top_q1e8 (tail results) (flipped_results ++ [(flip_top_q1e8 (head results))])
        
-- Horizontally flips all the boards in the given result list
flip_results_left_q1e8 :: [[String]] -> [[String]] -> [[String]]
flip_results_left_q1e8 results flipped_results
        | null results        = flipped_results
        | otherwise                = flip_results_left_q1e8 (tail results) (flipped_results ++ [(flip_left_q1e8 (head results))])

-- Given a board, flip it left to right.
-- Call example: flip_left_q1e8 ["w---","-w-","--","b--","-bbb"]
-- -> returns ["---w","-w-","--","--b","bbb-"]
flip_left_q1e8 :: [String] -> [String]
flip_left_q1e8 [] = []
flip_left_q1e8 (x:xs) = (reverse x) : (flip_left_q1e8 xs) 

-- Given an array of strings, replace all b's for w's and all w's for b's.
swap_pieces_q1e8 :: [String] -> [String]
swap_pieces_q1e8 [] = []
swap_pieces_q1e8 (x:xs) = (swap_row_pieces_q1e8 x) : (swap_pieces_q1e8 xs)

-- Given a String, replace all b's for w's and all w's for b's.
swap_row_pieces_q1e8 :: String -> String
swap_row_pieces_q1e8 [] = []
swap_row_pieces_q1e8 (x:xs)
        | x == 'w'      = 'b' : (swap_row_pieces_q1e8 xs)
        | x == 'b'      = 'w' : (swap_row_pieces_q1e8 xs)
        | otherwise     =  x  : (swap_row_pieces_q1e8 xs)
        
-- Return -1 if true. 1 if False.
negate_q1e8 :: Bool -> Int
negate_q1e8 b
        | b     = -1
        | otherwise = 1

{----------------------------------------------------------------
------------------------ VALIDATING BOARD -----------------------
-----------------------------------------------------------------}

-- Given a board, check if the format is valid.
-- @return false if the format invalid. True if all tests passed.
-- Call example: valid_board_q1e8 ["wwww","---","--","---","bbbb"]
valid_board_q1e8 :: [String] -> Bool
valid_board_q1e8 board
        | null board                                    = False
        | (length board) < 4                            = False
        | even (length board)                           = False
        | (board_size_q1e8 board) /= (length board)     = False
        | not (hourglass_shape_q1e8 board)              = False
        | invalid_pieces_q1e8 board                     = False
        | otherwise                                     = True

-- @return: the expected board size = (2 * n) - 3
-- where n is the length of the first string in the array of strings.
-- Call example: board_size_q1e8 ["wwww","---","--","---","bbbb"] -> 5
board_size_q1e8 :: [String] -> Int
board_size_q1e8 board = (length (head board))*2 - 3

-- @param board: the board that will be checked
-- @return: true if the board has an hourglass format (eg: 4 3 2 3 4).
hourglass_shape_q1e8 :: [String] -> Bool
hourglass_shape_q1e8 board@(x:xs)
        | null board                = False
        | firstL < 4                = False
        | expectedFirstL /= firstL  = False
        | otherwise                 = isHourglass
        where firstL = length x
              expectedFirstL = starting_pieces_q1e8 board
              half = (div (length board) 2) - 1
              isHourglass = hourglass_helper_q1e8 xs half (firstL-1)
hourglass_helper_q1e8 :: [String] -> Int -> Int -> Bool
hourglass_helper_q1e8 [] 0 _ = True        
hourglass_helper_q1e8 [] _ _ = False
hourglass_helper_q1e8 (x:xs) 0 rowLength
        | not (validate_length_q1e8 x rowLength)   = False
        | otherwise                     = 
                hourglass_helper_q1e8 xs 0 (rowLength+1)                        
hourglass_helper_q1e8 (x:xs) distToCenter rowLength
        | not (validate_length_q1e8 x rowLength)   = False
        | otherwise                     = 
                hourglass_helper_q1e8 xs (distToCenter-1) (rowLength-1)
 
-- Receive a string and a length and make sure that string has that same length.
-- Also, make sure the string has at least 2 chars.
-- @return false otherwise.
validate_length_q1e8 :: String -> Int -> Bool
validate_length_q1e8 str expected = ((length str) == expected) && (length str > 1)

 -- Given a board, return the max number of pieces each player can start with.    
starting_pieces_q1e8 :: [String] -> Int
starting_pieces_q1e8 board = div (length board + 3) 2

-- Given a board, check if all pieces are 'w', 'b' or '-' 
-- and their numbers are within limits.
-- @return true otherwise
invalid_pieces_q1e8 :: [String] -> Bool    
invalid_pieces_q1e8 board
        | os > 0         = True -- There shouldn't be extra pieces
        | ws > maxPieces = True
        | bs > maxPieces = True
        | otherwise      = False
        where (ws, bs, os) = pieces_counter_board_q1e8 board 0 0 0
              maxPieces = starting_pieces_q1e8 board

-- Count the number of pieces in the given string.
-- @return a tuple (w, b, o) with the num of pieces for each side + the
-- initial values for w, b, and o. The third parameter of the tuple is the
-- number of things that are not pieces nor space.
-- Call example: pieces_counter_q1e8 "w--bb-3Xb" 0 0 0 -> returns (1, 3, 2)
pieces_counter_q1e8 :: String -> Int -> Int -> Int -> (Int, Int, Int)
pieces_counter_q1e8 [] w b o = (w, b, o)
pieces_counter_q1e8 ('w':xs) w b o = pieces_counter_q1e8 xs (w+1) b o
pieces_counter_q1e8 ('b':xs) w b o = pieces_counter_q1e8 xs w (b+1) o
pieces_counter_q1e8 ('-':xs) w b o = pieces_counter_q1e8 xs w b o
pieces_counter_q1e8 (x:xs) w b o = pieces_counter_q1e8 xs w b (o+1)

-- Similar to the pieces_counter function. 
-- However, this one is the total sum for all the board.
pieces_counter_board_q1e8 :: [String] -> Int -> Int -> Int -> (Int, Int, Int)
pieces_counter_board_q1e8 [] w b o = (w, b, o)
pieces_counter_board_q1e8 (x:xs) w b o = pieces_counter_board_q1e8 xs nw nb no
        where (nw, nb, no) = pieces_counter_q1e8 x w b o
       
-- Return the number of b pieces in a row 
b_row_count_q1e8 :: String -> Int
b_row_count_q1e8 row = bs
        where (_, bs, _) = pieces_counter_q1e8 row 0 0 0

-- Return the number of w pieces in the board
-- Call example: w_count_q1e8 ["ww--","-w-","--","---","bbbb"] -> 3       
w_count_q1e8 :: [String] -> Int
w_count_q1e8 board = ws
        where (ws, _, _) = pieces_counter_board_q1e8 board 0 0 0

-- Return the number of b pieces in the board   
-- Call example: b_count_q1e8 ["ww--","-w-","--","---","bbbb"] -> 4  
b_count_q1e8 :: [String] -> Int
b_count_q1e8 board = bs
        where (_, bs, _) = pieces_counter_board_q1e8 board 0 0 0
{----------------------------------------------------------------
------------------------- PRINT FUNCTIONS ----------------------
-----------------------------------------------------------------}

-- @param board: the board that will be printed. Prints in the console.
-- Call example 1: print_q1e8 ["ww-","-w","bbb"]
-- Call example 2: print_q1e8 ["w-w-w","--w-","w--","--","---","--bb","bb-b-"]
-- print_q1e8 ["wwwwww","-----","----", "-T-", "--", "---","----","-----","bbbbbb"]
print_q1e8 :: [String] -> IO ()
print_q1e8 board
        | not (valid_board_q1e8 board)  = putStrLn "** INVALID BOARD **"
        | otherwise = print_board_q1e8 board 1 (length board) 0 (-1)

-- @param boards: an array with boards. This function will print all of them.
print_boards_q1e8 :: [[String]] -> IO ()
print_boards_q1e8 [] = putStrLn ""
print_boards_q1e8 (x:xs) = do
        print_q1e8 x
        putStrLn ""
        print_boards_q1e8 xs
        
-- Print helper to print the board on console. Magic happens here.
print_board_q1e8 :: [String] -> Int -> Int -> Int -> Int -> IO ()
print_board_q1e8 [] _ _ previousLength previousIndent = 
        print_line_q1e8 previousLength previousIndent (even previousIndent)
print_board_q1e8 (x:xs) currentRow totalRows previousLength previousIndent = do
        print_line_q1e8 (max (length x) previousLength) lineIndent (even lineIndent)
        print_row_q1e8 x indent
        print_board_q1e8 xs (currentRow + 1) totalRows (length x) indent
        where indent = next_indent_q1e8 previousIndent totalRows currentRow
              lineIndent = max indent previousIndent

-- @return the next indent based on the idea that the indent is getting
-- bigger until half of the board. Then, it starts to shrink.
next_indent_q1e8 :: Int -> Int -> Int -> Int
next_indent_q1e8 previousIndent totalRows currentRow
        | currentRow > ((div (totalRows+1) 2))  = previousIndent - 1
        | otherwise                             = previousIndent + 1
        
-- @param size: the size of the row of lines (about 4 times that)
-- @param indent: the space before printing the row of lines
-- @param evenIndent: helper to determine if an extra '-' or space is required
-- Call example: print_line_q1e8 3 2 True -> prints "  -------------"
print_line_q1e8 :: Int -> Int -> Bool -> IO ()
print_line_q1e8 0 indent evenIndent
        | evenIndent = putStrLn "-"
        | otherwise = putStrLn ""
print_line_q1e8 size 0 evenIndent = do
        putStr "----"
        print_line_q1e8 (size-1) 0 evenIndent
print_line_q1e8 size 1 False = do
        putStr "-"
        print_line_q1e8 size 0 False
print_line_q1e8 size 1 True = print_line_q1e8 size 0 True
print_line_q1e8 size indent evenIndent = do
        putStr "  "
        print_line_q1e8 size (indent - 1) evenIndent
        
-- @param str: a string that will be printed in the console
-- @param indent: how much space should be printed before printing str.
-- Call example 1: print_row_q1e8 "wwww" 3 -> prints "      | w | w | w | w |"
print_row_q1e8 :: String -> Int -> IO ()
print_row_q1e8 [] _ = putStrLn "|"
print_row_q1e8 (x:xs) 0 = do 
        print_piece_q1e8 x
        print_row_q1e8 xs 0  
print_row_q1e8 str indent = do 
        putStr "  "
        print_row_q1e8 str (indent - 1)

-- Helper of the print function that prints a small portion of a row       
print_piece_q1e8 :: Char -> IO ()
print_piece_q1e8 'w' = putStr "| w "
print_piece_q1e8 'b' = putStr "| b "
print_piece_q1e8 '-' = putStr "|   "
print_piece_q1e8  c  = putStr "| ? "
         