import qualified Cells.Cell

module Boards (Board(Cell), createBoard, getLinha, getColuna) where
    data Board = Board [[Cell]]
        deriving Show

    type Cell = Int Bool

    createBoard :: [[Cell]] -> Board
    createBoard m = Board m

    --  otimizacao
    getLinha :: Board -> Int -> [Cell]
    getLinha lineNumber b = b !! lineNumber

    getColuna :: Board -> Int -> [Cell]
    getColuna b n = map (!! n)