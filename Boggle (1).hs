module Boggle (boggle) where
import Data.List (foldr, nubBy)

-- | 'boggle' function takes a Boggle board and a list of words to search for.
-- It returns a list of tuples where each tuple contains a word and its corresponding list of coordinates on the board where the word is found.
boggle :: [String] -> [String] -> [(String, [(Int, Int)])]
boggle board words =
    let boardSize = length board
        -- Apply foldr to the list of words to find all the words on the board.
        foundWords = foldr (\word res ->
            -- Check if the length of the word is less than or equal to the total number of cells in the board.
            if length word <= boardSize * boardSize
                then let listOfChars = word
                         -- Find the first character of the word on the board.
                         points = findFirst board word boardSize listOfChars
                     in if not (null points)
                            -- If the word is found, add it to the result.
                            then (word, points) : res
                            -- Otherwise, continue with the next word.
                            else res
                else res
            ) [] words
    -- Remove duplicate words from the result.
    in nubBy (\(w1, _) (w2, _) -> w1 == w2) foundWords

-- | 'findFirst' function finds the starting position of a word on the board.
findFirst :: [String] -> String -> Int -> String -> [(Int, Int)]
findFirst board word boardSize wordChars =
    let firstChar = head wordChars
        -- Find all occurrences of the first character of the word on the board.
        visited = [(row, column) | row <- [0..(boardSize - 1)], column <- [0..(boardSize - 1)], ((board !! row) !! column) == firstChar]
    in findAllPossiblePaths board word boardSize visited wordChars []

-- | 'findAllPossiblePaths' function explores all possible paths to find the word on the board.
findAllPossiblePaths :: [String] -> String -> Int -> [(Int, Int)] -> String -> [(Int, Int)] -> [(Int, Int)]
findAllPossiblePaths _ _ _ [] _ res = res
findAllPossiblePaths board word boardSize ((row, column):tail) listOfChars listOfPoints =
    let points = findPoints board boardSize row column listOfChars []
    in if not (null points)
       then listOfPoints ++ points
       else findAllPossiblePaths board word boardSize tail listOfChars listOfPoints

-- | 'findPoints' function recursively explores neighboring cells to find the entire word on the board.
findPoints :: Eq a => [[a]] -> Int -> Int -> Int -> [a] -> [(Int, Int)] -> [(Int, Int)]
findPoints board boardSize row column [] points = points
findPoints board boardSize row column (head:tail) points
    -- Base case: If any of the conditions fail, return an empty list.
    | row < 0 || row >= boardSize || column < 0 || column >= boardSize || (row, column) `elem` points || (board !! row) !! column /= head = []
    -- Recursive case: Explore neighboring cells to find the entire word.
    | otherwise =
        let newPoints = points ++ [(row, column)]
            -- Explore all possible paths from the current cell.
            results = [ findPoints board boardSize (row-1) column tail newPoints,
                        findPoints board boardSize (row+1) column tail newPoints,
                        findPoints board boardSize row (column-1) tail newPoints,
                        findPoints board boardSize row (column+1) tail newPoints,
                        findPoints board boardSize (row-1) (column+1) tail newPoints,
                        findPoints board boardSize (row-1) (column-1) tail newPoints,
                        findPoints board boardSize (row+1) (column-1) tail newPoints,
                        findPoints board boardSize (row+1) (column+1) tail newPoints ]
            -- Choose the first non-empty result as the final result.
            result = case filter (not . null) results of
                      (res:_) -> res
                      _ -> []
        in result
