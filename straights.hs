import Boards

main = do

    let bordinho = [[(0, True, 0, 0), (0, True, 0, 1), (0, True, 0, 2)],
                    [(0, True, 1, 0), (0, True, 1, 1), (0, True, 1, 2)],
                    [(0, True, 2, 0), (0, True, 2, 1), (0, True, 2, 2)]]

    let teste = [[(0, False, 0, 0), (0, True, 0, 1), (0, True, 0, 2), (0, False,0, 3), (0, False,0, 4), (0, False, 0, 5)],
                  [(0, False, 1, 0), (0, True, 1, 1), (0, True, 1, 2), (0, True, 1, 3), (0, True, 1, 4), (0, True,  1, 5)],
                  [(0, False, 2, 0), (0, True, 2, 1), (0, True, 2, 2), (0, True, 2, 3), (0, True, 2, 4), (0, True , 2, 5)],
                  [(0, True,  3, 0), (0, True, 3, 1), (0, True, 3, 2), (0, True, 3, 3), (0, True, 3, 4), (0, False, 3, 5)],
                  [(0, True,  4, 0), (0, True, 4, 1), (0, True, 4, 2), (0, True, 4, 3), (0, True, 4, 4), (0, False, 4, 5)],
                  [(0, False, 5, 0), (0, False,5, 1), (0, False,5, 2), (0, True, 5, 3), (0, True, 5, 4), (0, False, 5, 5)]]

    let puzzle = [[(0, False, 0, 0), (0, True, 0, 1), (0, True, 0, 2), (1, False,0, 3), (0, False,0, 4), (0, False, 0, 5)],
                  [(0, False, 1, 0), (0, True, 1, 1), (0, True, 1, 2), (0, True, 1, 3), (5, True, 1, 4), (0, True,  1, 5)],
                  [(0, False, 2, 0), (0, True, 2, 1), (1, True, 2, 2), (0, True, 2, 3), (0, True, 2, 4), (0, True , 2, 5)],
                  [(4, True,  3, 0), (0, True, 3, 1), (0, True, 3, 2), (0, True, 3, 3), (0, True, 3, 4), (0, False, 3, 5)],
                  [(0, True,  4, 0), (6, True, 4, 1), (5, True, 4, 2), (0, True, 4, 3), (0, True, 4, 4), (0, False, 4, 5)],
                  [(0, False, 5, 0), (0, False,5, 1), (0, False,5, 2), (0, True, 5, 3), (1, True, 5, 4), (4, False, 5, 5)]]


    let solution = [[(0, False, 0, 0), (4, True, 0, 1), (3, True, 0, 2), (1, False,0, 3), (0, False,0, 4), (0, False, 0, 5)],
                  [(0, False, 1, 0), (2, True, 1, 1), (4, True, 1, 2), (3, True, 1, 3), (5, True, 1, 4), (1, True,  1, 5)],
                  [(0, False, 2, 0), (3, True, 2, 1), (1, True, 2, 2), (5, True, 2, 3), (4, True, 2, 4), (2, True , 2, 5)],
                  [(4, True,  3, 0), (5, True, 3, 1), (2, True, 3, 2), (6, True, 3, 3), (3, True, 3, 4), (0, False, 3, 5)],
                  [(3, True,  4, 0), (6, True, 4, 1), (5, True, 4, 2), (4, True, 4, 3), (2, True, 4, 4), (0, False, 4, 5)],
                  [(0, False, 5, 0), (0, False,5, 1), (0, False,5, 2), (2, True, 5, 3), (1, True, 5, 4), (4, False, 5, 5)]]

    let cell = (0,True,1,0)

    -- putStrLn(show (setValue 5 cell))

    putStrLn (show (isValidBoard puzzle)) -- False
    putStrLn (show (isValidBoard solution)) -- True

    putStrLn (show (isValidBoardNoSequence puzzle)) -- True
    putStrLn (show (isValidBoardNoSequence puzzle)) -- True
    putStrLn (show (getCell puzzle (4,1)))-- Returns cell
    putStrLn (show (getColor(getCell puzzle (4,1)) == True && getValue(getCell puzzle (4,1)) == 0)) -- Returns the bool (white or not) of the cell
    putStrLn (show (getNextWhite puzzle (0,0)))
    putStrLn (show (itoxy ((xytoi (2,0)) + 1)))
    -- putStrLn (show (tryWithCond 4 teste (3,1)))
    putStrLn (show (tryWith 4 bordinho (2,1)))
    --putStrLn (show ((respectsColumnNoSequence solution (getCell solution (1,2))) == True)
    --putStrLn (show (setValue 2 (getCell puzzle (1,2)))) 
    -- putStrLn (show (splitAt (1) (getRow puzzle 2)))