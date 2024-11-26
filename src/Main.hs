import GameField
import LevelLoader

main :: IO ()
main = do
    putStrLn "Choose level 1 - 15:"
    level <- getLine
    gameField <- loadLevel (chooseLevel level)
    printGameField gameField