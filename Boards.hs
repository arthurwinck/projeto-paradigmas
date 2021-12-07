
module Boards (Board, Cell, respectsColumn, respectsColumnNoSequence) where
    import Data.List
    
    -- the type Cell contains a 1 >= x >= N value, a boolean informing whether
    -- its a black or white cell, and information on its row and column index
    type Cell = (Int, Bool, Int, Int)

    -- in Straights a Board is just a NxN matrix of Cells
    type Board = [[Cell]]



    -- retrieves a row by its index number
    -- @arguments:
    --    Board b - the board containing the puzzle
    --    Int n   - the number of the row to be returned
    -- @returns:
    --    a list of cells corresponding to the row on the puzzle
    getRow :: Board -> Int -> [Cell]
    getRow b n = b !! n



    -- retrieves a column by its index number
    -- @arguments:
    --    Board b - the board containing the puzzle
    --    Int n   - the number of the column to be returned
    -- @returns:
    --    a list of cells corresponding to the column on the puzzle             
    getColumn :: Board -> Int -> [Cell]
    getColumn b n = map (!! n) b



    -- retrieves the color from a cell
    -- @arguments:
    --     Cell (_, c, _, _) - the cell containing the color
    -- @returns:
    --     the color of the cell
    getColor :: Cell -> Bool
    getColor (_, c, _, _) = c



    -- retrieves the value (apparent to the player) from a cell
    -- @arguments:
    --     Cell (v, _, _, _) - the cell containing the value
    -- @returns:
    --     the value of the cell
    getValue :: Cell -> Int
    getValue (v,_,_,_) = v



    -- retrieves the values from a given row/column
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      a list containing the values of the cells
    getValues :: [Cell] -> [Int]
    getValues l = map getValue l




    -- filters cells that contain zero from a given row/column
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      the list of values of the cells
    filterZeros :: [Cell] -> [Cell]
    filterZeros l = filter (\(x,_,_,_) -> x /= 0) l



    -- retrieves the values (apparent to the player) from a list of cells
    -- zeroed and black cells are filtered. useful for checking sequences
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      the list of values of the cells
    filterZerosBlacks :: [Cell] -> [Cell]
    filterZerosBlacks l = filterZeros (filter (\(_,c,_,_) -> c) (l))



    -- checks if the list has repetitions
    -- @arguments:
    --    [Cell] l - list of Cells that needs to be checked
    -- @returns:
    --    True if the list has no repeated elements, False otherwise
    hasNoRepetitions :: [Int] -> Bool
    hasNoRepetitions l = length (nub l) == (length l)



    -- checks if the given list follows a sequence, that is, if each element n
    -- from the list has its predecessor and successor also on the list, with
    -- the exception of the greater and the smaller elements
    -- @arguments:
    --     List l - the list to be checked
    -- @returns:
    --     True if the elements on the list are a sequence, False otherwise
    respectsSequence :: [Cell] -> Bool
    respectsSequence l = all (`elem` (getValues (filterZerosBlacks l))) [minimum (getValues (filterZerosBlacks l)) .. maximum (getValues(filterZerosBlacks l))]



    -- checks whether there are any repeated numbers in this row
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, i, _) = takes the row index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsRow :: Board -> Cell -> Bool
    respectsRow b (_, _, i, _) = ((hasNoRepetitions (getValues (filterZeros (getRow b i))))
                                && respectsSequence (getColumn b i))


    
    -- checks whether there are any repeated numbers in this row, without 
    -- checking if the sequence is followed
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, i, _) = takes the row index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsRowNoSequence :: Board -> Cell -> Bool
    respectsRowNoSequence b (_, _, i, _) = hasNoRepetitions (getValues (filterZeros (getRow b i)))



    -- checks whether there are any repeated numbers in this column
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, _, i) = takes the column index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsColumn :: Board -> Cell -> Bool
    respectsColumn b (_, _, _, i) = ((hasNoRepetitions (getValues (filterZeros (getColumn b i))))
                                    && respectsSequence (getColumn b i))



    -- checks whether there are any repeated numbers in this column, without 
    -- checking if the sequence is followed
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, _, i) = takes the column index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsColumnNoSequence :: Board -> Cell -> Bool
    respectsColumnNoSequence b (_, _, _, i) = hasNoRepetitions (getValues (filterZeros (getColumn b i)))



    -- checks if the cell respects the rules of the game
    -- @arguments:
    --     Board b - the Board that contains the cell
    --     Cell c - the Cell to be verified
    -- @returns:
    --     True if the cell respects all the three rules, False otherwise
    isValidCell :: Board -> Cell -> Bool
    isValidCell b c = (respectsRow b c) && (respectsColumn b c)



    -- checks if the current position of board is valid
    -- @arguments:
    --     Board b - the board to be checked
    -- @returns:
    --     True if the rules are followed for all the cells, False otherwise
    -- isValidBoard :: Board -> Bool
    -- isValidBoard b = elem False (map isValidCell (map all b))