-- ** Rodrigo Alves **
-- ** This program gives one possible solution for the rush_hour game **

{- Examples:
  Medium level problem from thinkfun.com:
  Try it: rush_print ["-AAB--","-C-BD-","-CXXDE","IIH-DE","--HFFF","GGG---"] 
                   
                   ["-AAB--",
                    "-C-BD-",
                    "-CXXDE",
                    "IIH-DE",
                    "--HFFF",
                    "GGG---"] 
-}

--------- MAIN FUNCTION: ---------

-- Find the solution for the rush_hour problem. Exit is in the third row
-- '-' means free space and XX is the main vehicle in the third row.
-- Works for any size. The goal lane is always the middle one.
-- Cars and vehicles can be any character that is not - or X or repetitive
-- returns a path with a possible solution (not necessarily optimal).
rush_hour :: [String] -> [[String]]
rush_hour start = rush_hour_helper start True

-- Use this one if you don't want to use the heuristic function:
rush_hour_brute_force :: [String] -> [[String]]
rush_hour_brute_force start = rush_hour_helper start False

-- Solve rush_hour, but outputs nicely in the console
-- Eg: rush_print ["--B---","--B---","XXB---","--AA--","------","------"]
--     returns the solution in 9 steps
rush_print :: [String] -> IO () 
rush_print start = do
                printStrMatrix solution
                putStr "Number of steps to goal: "
                putStrLn (show (length solution - 1))
                where solution = rush_hour start
        
       
------------ HEURISTIC: ------------ 
        
-- MergeSort the list based on the heuristic function; 
-- so that, better states are checked first.
hSort :: [[String]] -> [[String]]
hSort states = sortedStates
        where goalLane = (length (head states) - 1) `div` 2
              zippedList = zip states (map (heuristic goalLane) states)
              sortedList = hSortHelper zippedList
              (sortedStates, _) = unzip sortedList

hSortHelper :: [(a, Int)] -> [(a, Int)]
hSortHelper [] = []
hSortHelper [a] = [a]
hSortHelper list = hMerge (hSortHelper l1) (hSortHelper l2)
        where (l1, l2) = split list

-- Bigger heuristic means further away from the goal state
-- This heuristic function starts at 0 and
-- adds +1 for each car on the way
heuristic :: Int -> [String] -> Int
heuristic goalLane state  = 
        heuristicHelper (reverse (getRow goalLane state)) 0
       
heuristicHelper :: String -> Int -> Int 
heuristicHelper [] h = h
heuristicHelper ('X':xs) h = h
heuristicHelper ('-':xs) h = heuristicHelper xs h
heuristicHelper (x:xs) h = heuristicHelper xs (h+1)

----------- SEARCHING: -----------

-- State space search algorithm. DFS with cycles.
statesearch :: [[String]] -> Int -> Bool -> [[String]] -> [[String]]
statesearch [] _ _ _ = [] -- There's nothing else to explore. No result
statesearch (x:xs) goalLane h path -- x is the head of the unexplored nodes
   | isGoal x goalLane = x:path -- Found goal: add to the path and return it
   | elem x path = statesearch xs goalLane h path -- Backtrack if repeated
   | (not (null result)) = result -- Found a result. Return it
   | otherwise = statesearch xs goalLane h path -- No result found. Backtrack
     where result = statesearch (generateNewStates x h) goalLane h (x:path)
     
-- @return true if the middle row ends with "XX"   
isGoal :: [String] -> Int -> Bool                    
isGoal state goalLane = (elemAt str (last-1) 'X') && (elemAt str last 'X')
        where str = getRow goalLane state
              last = (length str) - 1

rush_hour_helper :: [String] -> Bool -> [[String]]              
rush_hour_helper start useHeuristic =
        reverse (statesearch [start] goalLane useHeuristic [])
        where goalLane = (length start - 1) `div` 2
                     
------------ GENERIC HELPERS: ------------ 

-- Get a list of lists of strings and output them nicely.      
printStrMatrix :: [[String]] -> IO ()        
printStrMatrix [] = printStrList []
printStrMatrix (x:xs) = do
        printStrList x
        printStrMatrix xs

-- Print nicely a list of strings. 
-- Eg: printStrList ["aabb", "ccdd", "eeff"] prints in the console:
-- aabb
-- ccdd
-- eeff
printStrList :: [String] -> IO ()
printStrList [] = putStrLn ""
printStrList (x:xs) = do 
        putStrLn x
        printStrList xs                       
                       
-- Get a row number as a parameter and return that row from a matrix
-- Eg: getRow 3 ["abc", "def", "ghi", "jkl"] returns "jkl"
getRow ::  Int -> [[a]] -> [a]
getRow row matrix 
        | null matrix   = []
        | row == 0      = (head matrix)
        | otherwise     = getRow (row - 1) (tail matrix) 

-- Check if @param value is in array at index @param index. 
-- @return true if they are the same.
-- Eg: elemAt "abcd" 3 'd' returns True because 'd' is at index 3.  
elemAt :: (Eq a) => [a] -> Int -> a -> Bool      
elemAt [] _ _ = False
elemAt (x:xs) index value
        | index == 0    = (x == value)
        | otherwise     = elemAt xs (index-1) value

-- Receive a list and 2 indexes and swap the elements at those indexes.
-- Eg: swap "abc" 1 2 returns "acb"
swap :: [a] -> Int -> Int -> [a]
swap list i1 i2 = replace newList i1 (list!!i2)
             where newList = replace list i2 (list!!i1) 

-- Merge 2 sorted arrays into one array (they are all in ascending order)
-- They are merged based on the value of the tuple
-- Eg: hMerge [('a', 4), ('b', 7)] [('c', 2), ('d', 3)]
--     returns [('c',2),('d',3),('a',4),('b',7)]
hMerge :: [(a, Int)] -> [(a, Int)] -> [(a, Int)]
hMerge list1 list2 = mergeHelper list1 list2 []

mergeHelper :: [(a, Int)] -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]        
mergeHelper [] l2 result = reverse result ++ l2
mergeHelper l1 [] result = reverse result ++ l1
mergeHelper l1@(x:xs) l2@(y:ys) result
        | v1 < v2 = mergeHelper xs l2 (x:result)
        | otherwise = mergeHelper l1 ys (y:result)
        where (_, v1) = x
              (_, v2) = y 
        
-- Get a list and split it evenly. Return a tuple with the 2 halves.
-- Eg: split [1,4,8,16,32,64,128] returns ([1,8,32,128],[4,16,64])
split :: [a] -> ([a], [a])        
split list = splitHelper list [] []
splitHelper :: [a] -> [a] -> [a] -> ([a], [a])
splitHelper [] h1 h2 = (reverse h1, reverse h2)
splitHelper (x:xs) h1 h2 = splitHelper xs (x:h2) h1      
        
-- Replace an element in the list at index @param index to @param value
-- Eg: replace "abc" 1 'd' returns "adc"
replace :: [a] -> Int -> a -> [a]
replace list index value = replaceHelper list index value []
replaceHelper :: [a] -> Int -> a -> [a] -> [a]
replaceHelper [] _ _ result = reverse result
replaceHelper (x:xs) 0 value result = (reverse (value:result)) ++ xs
replaceHelper (x:xs) index value result = 
        replaceHelper xs (index - 1) value (x:result)

-- Rotate a matrix, so that its rows become its columns.
transpose :: [[a]] -> [[a]]     
transpose [] = []
transpose ([]:_) = []
transpose m = [x | (x:xs) <- m] : transpose ([xs | (_:xs) <- m])

-- Get a matrix and a list of rows. Replace the rowNumth row
-- at the matrix by all possible rows.
-- Eg: combine ["ab", "cd"] ["ef", "gh"] 1 returns [["ab", "gh"], ["ab", "ef"]] 
combine :: [a] -> [a] -> Int -> [[a]]    
combine matrix rows rowNum = combHelper matrix rows rowNum []
combHelper :: [a] -> [a] -> Int -> [a] -> [[a]]               
combHelper [] rows rowNum previous = []
combHelper (x:xs) rows 0 previous = combHelper2 p rows xs []
        where p = reverse previous
combHelper (x:xs) rows rowNum previous = 
        combHelper xs rows (rowNum-1) (x:previous)
combHelper2 :: [a] -> [a] -> [a] -> [[a]] -> [[a]]
combHelper2 previous [] after results = results
combHelper2 previous (x:xs) after results =
        combHelper2 previous xs after ((previous ++ x:after) : results)


--------- GENERATING NEW STATES: ---------

-- Find all moves possible
generateNewStates :: [String] -> Bool -> [[String]]
generateNewStates currState heuristic
        | heuristic     = hSort possibleStates
        | otherwise     = possibleStates
        where possibleStates = concat [horizontalMoves currState, 
                                       verticalMoves currState]
-- Find all vertical moves possible
verticalMoves :: [String] -> [[String]]
verticalMoves currState = map transpose (horizontalMoves (transpose currState))

-- Find all horizontal moves possible
horizontalMoves :: [String] -> [[String]]
horizontalMoves currState = horizontalMovesHelper currState currState 0 []
       
horizontalMovesHelper :: [String] -> [String] -> Int -> [[String]] -> [[String]]
horizontalMovesHelper _ [] _ states = states              
horizontalMovesHelper matrix (x:xs) rowNum states =
        horizontalMovesHelper matrix xs (rowNum + 1) newStates
                where rows = laneMoves x
                      newStates = (combine matrix rows rowNum) ++ states
            
-- Find all possible moves in a single lane (moves to left or to right)
laneMoves :: String -> [String]
laneMoves list = concat [movingToRight list, movingToLeft list]

-- Return an array with all possible moves from left to right in a lane
movingToRight :: String -> [String]
movingToRight list = map reverse (movingToLeft (reverse list))

-- Return an array with all possible moves from right to left in a lane
movingToLeft :: String -> [String]
movingToLeft list = movingHelper list [] []

movingHelper :: String -> String -> [String] -> [String]
movingHelper [] _ result = result
movingHelper [_] _ result = result
movingHelper ('-':x:xs) previous result
        | (x /= '-')&&(vSize > 1) = movingHelper xs (x:nextPrevious) newResult
        | otherwise               = movingHelper (x:xs) nextPrevious result
        where vSize = vehicleSize(x:xs)
              nextPrevious = '-' : previous
              newState = (reverse previous) ++ (swap ('-':x:xs) 0 vSize)
              newResult = newState : result
movingHelper (x:xs) previous result = movingHelper xs (x:previous) result
 
-- Get the size of the vehicle
-- Eg: "CCC----DD" returns 3 because vehicle C has size 3
vehicleSize :: Eq a => [a] -> Int
vehicleSize front = (seqEndIndex front 0) + 1
              
-- Returns the index of the last element of a sequence of same values 
-- that starts at @param startIndex.
-- Eg: seqEndIndex "aab" 0 returns 1 because it's the index of the last a.
-- Eg 2: seqEndIndex "abbbbbbc" 2 returns 6 
seqEndIndex :: Eq a => [a] -> Int -> Int
seqEndIndex list startIndex = seqEndHelper (tail rest) (head rest) startIndex 
        where rest = drop startIndex list

seqEndHelper :: Eq a => [a] -> a -> Int -> Int        
seqEndHelper [] _ endIndex = endIndex
seqEndHelper (x:xs) value endIndex
        | x == value      = seqEndHelper xs value (endIndex+1)
        | otherwise       = endIndex 
        
