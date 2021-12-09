
module Boards (Board, Cell, isValidBoard, isValidBoardNoSequence, setValue) where
    import Data.List
    
    -- the type Cell contains a 1 >= x >= N value, a boolean informing whether
    -- its a black or white cell, and information on its row and column index
    type Cell = (Int, Bool, Int, Int)

    -- in Straights a Board is just a NxN matrix of Cells
    type Board = [[Cell]]



    -- HELPER FUNCTIONS
    -- functions that retrieve data or handle data in a useful way to many
    -- functionalities
    getColor :: Cell -> Bool
    getColumn :: Board -> Int -> [Cell]
    getRow :: Board -> Int -> [Cell]
    getValue :: Cell -> Int
    getValues :: [Cell] -> [Int]
    negator :: Bool -> Bool

    -- VERIFICATION FUNCTIONS
    -- Functions used for verifying if the board is currently in a consistent
    -- state
    filterZeros :: [Cell] -> [Cell]
    filterZerosBlacks :: [Cell] -> [Cell]
    hasNoRepetitions :: [Int] -> Bool
    isValidBoard :: Board -> Bool
    isValidBoardNoSequence :: Board -> Bool
    isValidCell :: Board -> Cell -> Bool
    isValidCellNoSequence :: Board -> Cell -> Bool
    respectsColumn :: Board -> Cell -> Bool
    respectsColumnNoSequence :: Board -> Cell -> Bool
    respectsRow :: Board -> Cell -> Bool
    respectsRowNoSequence :: Board -> Cell -> Bool
    respectsSequence :: [Cell] -> Bool



    -- retrieves the color from a cell
    -- @arguments:
    --     Cell (_, c, _, _) - the cell containing the color
    -- @returns:
    --     the color of the cell
    getColor (_, c, _, _) = c


    -- retrieves a column by its index number
    -- @arguments:
    --    Board b - the board containing the puzzle
    --    Int n   - the number of the column to be returned
    -- @returns:
    --    a list of cells corresponding to the column on the puzzle             
    getColumn b n = map (!! n) b


    -- retrieves a row by its index number
    -- @arguments:
    --    Board b - the board containing the puzzle
    --    Int n   - the number of the row to be returned
    -- @returns:
    --    a list of cells corresponding to the row on the puzzle
    getRow b n = b !! n


    -- retrieves the value (apparent to the player) from a cell
    -- @arguments:
    --     Cell (v, _, _, _) - the cell containing the value
    -- @returns:
    --     the value of the cell
    getValue (v,_,_,_) = v


    -- retrieves the values from a given row/column
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      a list containing the values of the cells
    getValues l = map getValue l


    -- Changes the value of a given cell
    setValue :: Int -> Cell -> Cell
    setValue num (a,b,c,d) = (num,b,c,d)
    -- changes the value of the parameter
    -- @arguments:
    --     Bool - the initial value
    -- @returns:
    --      the initial value negated
    negator True = False
    negator False = True


    -- filters cells that contain zero from a given row/column
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      the list of values of the cells
    filterZeros l = filter (\(x,_,_,_) -> x /= 0) l


    -- retrieves the values (apparent to the player) from a list of cells
    -- zeroed and black cells are filtered. useful for checking sequences
    -- @arguments:
    --     [Cell] l - the list of cells
    -- @returns:
    --      the list of values of the cells
    filterZerosBlacks l = filterZeros (filter (\(_,c,_,_) -> c) (l))


    -- checks if the list has repetitions
    -- @arguments:
    --    [Cell] l - list of Cells that needs to be checked
    -- @returns:
    --    True if the list has no repeated elements, False otherwise
    hasNoRepetitions l = length (nub l) == (length l)


    -- checks if the current position of board is valid
    -- @arguments:
    --     Board b - the board to be checked
    -- @returns:
    --     True if the rules are followed for all the cells, False otherwise
    isValidBoard b = negator (elem False (map (\c -> isValidCell b c) (foldl (++) [] b)))


    -- checks if the current position of board is valid, disregarding sequence
    -- @arguments:
    --     Board b - the board to be checked
    -- @returns:
    --     True if the rules are followed for all the cells, False otherwise
    isValidBoardNoSequence b = negator (elem False (map (\c -> isValidCellNoSequence b c) (foldl (++) [] b)))


    -- checks if the cell respects the rules of the game
    -- @arguments:
    --     Board b - the Board that contains the cell
    --     Cell c - the Cell to be verified
    -- @returns:
    --     True if the cell respects all the three rules, False otherwise
    isValidCell b c = (respectsRow b c) && (respectsColumn b c)


    -- checks if the cell respects the rules of the game, disregarding
    -- sequence
    -- @arguments:
    --     Board b - the Board that contains the cell
    --     Cell c - the Cell to be verified
    -- @returns:
    --     True if the cell respects all the three rules, False otherwise
    isValidCellNoSequence b c = ((respectsRowNoSequence b c)
                                && (respectsColumnNoSequence b c))


    -- checks whether there are any repeated numbers in this column
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, _, i) = takes the column index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsColumn b (_, _, _, i) = ((hasNoRepetitions (getValues (filterZeros (getColumn b i))))
                                    && respectsSequence (getColumn b i))


    -- checks whether there are any repeated numbers in this column, without 
    -- checking if the sequence is followed
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, _, i) = takes the column index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsColumnNoSequence b (_, _, _, i) = hasNoRepetitions (getValues (filterZeros (getColumn b i)))


    -- checks whether there are any repeated numbers in this row
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, i, _) = takes the row index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsRow b (_, _, i, _) = ((hasNoRepetitions (getValues (filterZeros (getRow b i))))
                                && respectsSequence (getColumn b i))


    -- checks whether there are any repeated numbers in this row, without 
    -- checking if the sequence is followed
    -- @arguments:
    --     Board b - the board containing the cell
    --     Cell (_, _, i, _) = takes the row index info from the cell
    -- @returns: 
    --     True if there are no repeated numbers, False otherwise
    respectsRowNoSequence b (_, _, i, _) = hasNoRepetitions (getValues (filterZeros (getRow b i)))


    -- checks if the given list follows a sequence, that is, if each element n
    -- from the list has its predecessor and successor also on the list, with
    -- the exception of the greater and the smaller elements
    -- @arguments:
    --     List l - the list to be checked
    -- @returns:
    --     True if the elements on the list are a sequence, False otherwise
    respectsSequence l = all (`elem` (getValues (filterZerosBlacks l))) [minimum (getValues (filterZerosBlacks l)) .. maximum (getValues(filterZerosBlacks l))]
