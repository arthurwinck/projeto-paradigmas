import Boards

main = do
    let puzzle = [[(0, False, 0, 0), (4, True, 0, 1), (3, True, 0, 2), (1, False,0, 3), (0, False,0, 4), (0, False, 0, 5)],
                  [(0, False, 1, 0), (2, True, 1, 1), (4, True, 1, 2), (3, True, 1, 3), (5, True, 1, 4), (1, True,  1, 5)],
                  [(0, False, 2, 0), (3, True, 2, 1), (1, True, 2, 2), (5, True, 2, 3), (4, True, 2, 4), (2, True , 2, 5)],
                  [(4, True,  3, 0), (5, True, 3, 1), (2, True, 3, 2), (6, True, 3, 3), (3, True, 3, 4), (0, False, 3, 5)],
                  [(3, True,  4, 0), (6, True, 4, 1), (5, True, 4, 2), (4, True, 4, 3), (2, True, 4, 4), (0, False, 4, 5)],
                  [(0, False, 5, 0), (0, False,5, 1), (0, False,5, 2), (2, True, 5, 3), (1, True, 5, 4), (4, False, 5, 5)]]

    let row = (getRow puzzle 0)

    let column = (getColumn puzzle 3)

    putStrLn (show (respectsSequence row))
    putStrLn (show (isValidCell puzzle (1, True, 5, 4)))
