module LevelLoader where

import Data.List
import GameField

-- Функция для нахождения позиции игрока при загрузке файла
findPlayerPosition :: Field -> ElementPosition
findPlayerPosition field =
    let playerRow = findIndex (elem Player) field
        playerCol = case playerRow of
            Just row -> elemIndex Player (field !! row)
            Nothing  -> Nothing
    in case (playerRow, playerCol) of
        (Just row, Just col) -> (row, col)
        _                    -> error "Player position not found"

-- Функция для получения списка меток
findGoalPositions :: GameField -> [ElementPosition]
findGoalPositions (GameField field _ ) = 
    [(row, col) | (row, line) <- zip [0..] field, (col, elem) <- zip[0..] line, elem == Goal || elem == BoxOnGoal]

-- Функция загрузки игрового уровня
loadLevel :: FilePath -> IO GameField
loadLevel file = do
    content <- readFile file
    let linesOfFile = lines content
    let field = map (map toGameElement) linesOfFile
    let playerPos = findPlayerPosition field
    return (GameField field playerPos)

-- Функция для выбора уровня
chooseLevel :: String -> FilePath
chooseLevel level = "../levels/" ++ level ++ ".txt"
