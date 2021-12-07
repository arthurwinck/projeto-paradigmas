
module Boards (Board, Cell, getRow, getColumn) where
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
