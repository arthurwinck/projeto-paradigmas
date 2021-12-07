
module Cells (Cell, createCell) where

    data Cell = Cell Int Bool
                deriving Show

    -- True -> branco
    -- False -> preto
    createCell :: Int -> Bool -> Cell
    createCell value isWhite = Cell value isWhite
