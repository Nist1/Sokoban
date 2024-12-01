module GameField where

-- Тип данных для представления элементов на игровом поле
data GameElement = Wall
                 | Player
                 | PlayerOnGoal
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
showElement Wall            = '#'
showElement Player          = '@'
showElement PlayerOnGoal    = '@'
showElement Box             = 'O'
showElement Goal            = 'x'
showElement BoxOnGoal       = 'G'
showElement Empty           = ' '

-- Функция для получения элемента на указанной позиции
getElement :: ElementPosition -> Field -> GameElement
getElement (row, col) field = (field !! row) !! col

-- Проверка, что элемент - игрок (@)
isPlayer :: GameElement -> Bool
isPlayer gameElem = gameElem == Player || gameElem == PlayerOnGoal

-- Проверка, что элемент - пустое место ( ./  )
-- Также используется для проверки, что на метку может пройти игрок
isEmpty :: GameElement -> Bool
isEmpty gameElem = gameElem == Empty || gameElem == Goal

-- Проверка, что элемент - ящик (О)
isBox :: GameElement -> Bool
isBox gameElem = gameElem == Box || gameElem == BoxOnGoal

-- Проверка, что элемент - метка (х) или игрок/ящик на метке
isGoal :: GameElement -> Bool
isGoal gameElem = gameElem == Goal || gameElem == BoxOnGoal || gameElem == PlayerOnGoal

type Line = [GameElement]         -- Тип для представления ряда игрового поля
type Field = [Line]               -- Тип для представления всего поля
type ElementPosition = (Int, Int) -- Тип для представления координат игрока на поле

-- Тип для представления игрового поля. Содержит поле и позицию игрока
data GameField = GameField {
    currentField :: Field,
    playerPosition :: ElementPosition
} deriving (Show)

-- Функция печати игрового поля в консоль
printGameField :: GameField -> IO ()
printGameField (GameField field _) = mapM_ (putStrLn . map showElement) field

-- Функция проверки, что уровень решен
-- Использована n-редукция для сравнения положения меток и положения ящиков 
isLevelSolved :: GameField -> [ElementPosition] -> Bool
isLevelSolved (GameField field _) = all (\(row, col) -> field !! row !! col == BoxOnGoal)
