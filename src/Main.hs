import Prelude hiding (Left, Right)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import System.Console.ANSI (clearScreen, setCursorPosition)

import GameField
import LevelLoader
import Move

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- Убирает ожидание символа конца строки
    hSetEcho stdin True
    putStrLn "Choose level 1 - 15:"
    l <- getLine
    let level = chooseLevel l
    gameField <- loadLevel level
    let goals = findGoalPositions gameField
    gameLoop level gameField goals

gameLoop :: FilePath -> GameField -> [ElementPosition] -> IO ()
gameLoop level gameField goals = do
    --putStrLn "\ESC[2J" (Должно работать на Linux (Либо искать способ очистки предыдущего вывода) | На Windows не работает)
    clearScreen
    setCursorPosition 0 0
    printGameField gameField
    if isLevelSolved gameField goals
        then putStrLn "Level solved successfully! Congratulations!"
        else do
            key <- getKey
            case key of
                'q' -> return () -- Выход из игры
                'r' -> do        -- Рестарт уровня
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
                            -- При неправильном вводе ничего не происходит,
                            -- поле выводится в том же состоянии
                            gameLoop level gameField goals

getKey :: IO Char
getKey = do
    hSetEcho stdin False
    c <- getChar
    case c of
        '\n' -> getKey  -- Игнорируем символ новой строки и повторяем чтение
        _    -> return c
