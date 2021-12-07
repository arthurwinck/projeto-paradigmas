import Boards

main = do
    let cell = (createCell 1 False)

    let puzzle = [[(createCell 0 False), (createCell 0 True), (createCell 0 True), (createCell 1 False), (createCell 0 False), (createCell 0 False)], [(createCell 0 False), (createCell 0 True), (createCell 0 True), (createCell 0 True), (createCell 5 True), (createCell 0 False)]]

    let line = (getLinha puzzle 0)

    putStrLn (show cell)
    putStrLn (show puzzle)
    putStrLn (show line)
