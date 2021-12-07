import Boards

main = do
    let cell = (1, False)

    let puzzle = [[(0, False, 0, 0), (0, True, 0, 1), (0, True, 0, 2), (1, False, 0, 3), (0, False, 0, 4), (0, False, 0, 5)], [(0, False, 1, 0), (0, True, 1, 1), (0, True, 1, 2), (0, True, 1, 3), (5, True, 1, 4), (0, False, 1, 5)]]

    let row = (getRow puzzle 0)

    let column = (getColumn puzzle 3)

    putStrLn (show cell)
    putStrLn (show puzzle)
    putStrLn (show row)
    putStrLn (show column)
