import Prelude hiding (Left, Right)
--import System.Console.Terminfo.Base
import System.IO (hSetEcho, stdin)


import GameField
import LevelLoader
import Move

main :: IO ()
main = do
    putStrLn "Choose level 1 - 15:"
    l <- getLine
    let level = chooseLevel l
    gameField <- loadLevel level
    let goals = findGoalPositions gameField
    --setupTerm
    gameLoop level gameField goals
    --cleanUpTerm

gameLoop :: FilePath -> GameField -> [ElementPosition] -> IO ()
gameLoop level gameField goals = do
    --putStrLn "\ESC[2J" (Должно работать на Linux (Либо искать способ очистки предыдущего вывода)| На Windows не работает)
    printGameField gameField
    if isLevelSolved gameField goals
        then putStrLn "Level solved successfully! Congratulations!"
        else do
            key <- getKey
            case key of
                'q' -> return ()
                'r' -> do
                    putStrLn "Restarting level..."
                    gameField <- loadLevel level
                    gameLoop level gameField goals
                _ -> do
                    let direction = case key of
                            'w' -> Just Up
                            's' -> Just Down
                            'a' -> Just Left
                            'd' -> Just Right
                            _   -> Nothing
                    case direction of
                        Just dir -> do
                            gameLoop level (handleMovement dir gameField) goals
                        Nothing  -> do
                            putStrLn "Invalid move. Use 'w', 'a', 's', 'd' keys."
                            gameLoop level gameField goals

getKey :: IO Char
getKey = do
    hSetEcho stdin False
    c <- getChar
    case c of
        '\n' -> getKey  -- Игнорируем символ новой строки и повторяем чтение
        _    -> return c

-- setupTerm :: IO ()
-- setupTerm = do
--     setupTermFromEnv
--     setEcho False
--     setCbreak True

-- cleanupTerm :: IO ()
-- cleanupTerm = do
--     setEcho True
--     setCbreak False
--     resetTerminalState
