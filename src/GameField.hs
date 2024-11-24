module GameField where

import Data.List

-- Тип данных для представления элементов на игровом поле
data GameElement = Wall
                 | Player
                 | Box
                 | Goal
                 | BoxOnGoal
                 | Empty
                 deriving (Eq, Show)

-- Функция для перевода символа в элемент игрового поля
toGameElement :: Char -> GameElement
toGameElement '#' = Wall
toGameElement '@' = Player
toGameElement 'O' = Box
toGameElement 'x' = Goal
toGameElement 'G' = BoxOnGoal
toGameElement  _  = Empty

-- Функция для печати элемента в консоль
showElement :: GameElement -> Char
showElement Wall      = '#'
showElement Player    = '@'
showElement Box       = 'O'
showElement Goal      = 'x'
showElement BoxOnGoal = 'G'
showElement Empty     = ' '

type Line = [GameElement]        -- Тип для представления ряда игрового поля
type Field = [Line]              -- Тип для представления всего поля
type PlayerPosition = (Int, Int) -- Тип для представления координат игрока на поле

-- Тип для представления игрового поля. Содержит поле и позицию игрока
data GameField = GameField {
    currentField :: Field,
    playerPosition :: PlayerPosition
} deriving (Show)

-- Функция для нахождения позиции игрока при загрузке файла
findPlayerPosition :: Field -> PlayerPosition
findPlayerPosition field =
    let playerRow = findIndex (elem Player) field
        playerCol = case playerRow of
            Just row -> elemIndex Player (field !! row)
            Nothing  -> Nothing
    in case (playerRow, playerCol) of
        (Just row, Just col) -> (row, col)
        _                    -> error "Player position not found"

-- Функция загрузки игрового уровня
loadLevel :: FilePath -> IO GameField
loadLevel file = do
    content <- readFile file
    let linesOfFile = lines content
    let field = map (map toGameElement) linesOfFile
    let playerPos = findPlayerPosition field
    return (GameField field playerPos)

-- Функция печати игрового поля в консоль
printGameField :: GameField -> IO ()
printGameField (GameField field _) = mapM_ (putStrLn . map showElement) field

main = do
    gameField <- loadLevel "../levels/1.txt"
    printGameField gameField