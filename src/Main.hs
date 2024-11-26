import GameField
import LevelLoader
import Move

main :: IO ()
main = do
    putStrLn "Choose level 1 - 15:"
    level <- getLine
    gameField <- loadLevel (chooseLevel level)
    let goals = findGoalPositions gameField
    printGameField gameField
