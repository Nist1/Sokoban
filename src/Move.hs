module Move where

import Prelude hiding (Left, Right)
import GameField

-- Тип для представления направления движения игрока
data MoveDirection = Up
                   | Down
                   | Left
                   | Right deriving (Eq)

-- Функция для получения элемента на указанной позиции
getElement :: ElementPosition -> Field -> GameElement
getElement (row, col) field = (field !! row) !! col

-- Вспомогательная функция для вычисления новой позиции элемента в соответствии с заданным направлением
advance :: ElementPosition -> MoveDirection -> ElementPosition
advance (row, col) Up    = (row - 1, col)
advance (row, col) Down  = (row + 1, col)
advance (row, col) Left  = (row, col - 1)
advance (row, col) Right = (row, col + 1)

-- Функция для проверки, возможно ли перемещение в заданном направлении 
canMoveTo :: MoveDirection -> GameField -> Bool
canMoveTo direction gameField =
    let field  = currentField gameField
        pos    = playerPosition gameField
        newPos = advance pos direction
    in  isEmpty $ getElement newPos field

-- Функция для проверки, возможно ли толкнуть ящик в заданном направлении
isPushable :: MoveDirection -> GameField -> Bool
isPushable direction gameField =
    let field        = currentField gameField
        pos          = playerPosition gameField
        playerNewPos = advance pos direction            -- Новая позиция игрока после передвижения
        e1           = getElement playerNewPos field
        boxNewPos    = advance playerNewPos direction   -- Новая позиция ящика после передвижения
        e2           = getElement boxNewPos field
    in  isBox e1 && isEmpty e2

-- Проверка, что элемент - игрок (@)
isPlayer :: GameElement -> Bool
isPlayer gameElem = gameElem == Player 

-- Проверка, что элемент - пустое место ( ./' '  )
-- Также используется для проверки, что на метку может пройти игрок
isEmpty :: GameElement -> Bool
isEmpty gameElem = gameElem == Empty || gameElem == Goal

-- Проверка, что элемент - ящик (О)
isBox :: GameElement -> Bool
isBox gameElem = gameElem == Box || gameElem == BoxOnGoal

-- Проверка, что элемент - метка (х)
isGoal :: GameElement -> Bool
isGoal gameElem = gameElem == Goal || gameElem == BoxOnGoal

-- Функция для обработки движения
handleMovement :: MoveDirection -> GameField -> GameField
handleMovement direction gameField
    | canMoveTo direction gameField  = move Player (playerPosition gameField) direction gameField
    | isPushable direction gameField = push direction gameField
    | otherwise                      = gameField

-- Функция для движения игрока или игрока вместе с ящиком 
-- Позиция игрока изменяется не зависимо от того, какой элемент двигается
-- при движении ящика функция вызывается снова, чтобы сдвинуть игрока
move :: GameElement -> ElementPosition -> MoveDirection -> GameField -> GameField
move gameElem pos direction gameField =
    let curPosition@(row, col)       = pos
        nextPostion@(newRow, newCol) = advance curPosition direction
        field                        = currentField gameField
        oldElement                   = getElement curPosition field  -- Запоминаем элемент на старой позиции
        newElement                   = getElement nextPostion field  -- Запоминаем элемент на новой позиции
        updatedField = if col == newCol
            then updateRow gameElem oldElement newElement (curPosition, nextPostion) field  -- Горизонтальное движение (Left, Right)
            else updateCol gameElem oldElement newElement (curPosition, nextPostion) field  -- Вертикальное движение (Up, Down)
    in  GameField {currentField = updatedField, playerPosition = nextPostion}

-- Функция передвижения ящика игроком
push :: MoveDirection -> GameField -> GameField
push direction gameField =
    let playerPos    = playerPosition gameField
        boxPos       = advance playerPos direction
        boxAfterPush = move Box boxPos direction gameField
    in move Player playerPos direction boxAfterPush

-- Функция для обновления строки игрового поля
updateRow :: GameElement -> GameElement -> GameElement -> (ElementPosition, ElementPosition) -> Field -> Field
updateRow gameElem oldElement newElement (oldPos@(row, col), newPos@(newRow, newCol)) field =
    let lineFrom        = field !! row
        lineTo          = field !! newRow
        newLineFrom     = updateAt col (if isGoal oldElement then Goal else Empty) lineFrom  -- Восстановление старого элемента на старой позиции
        newLineTo       = updateAt newCol (if isGoal newElement then BoxOnGoal else gameElem) lineTo  -- Установка нового элемента на новой позиции
        newField        = updateAt newRow newLineTo . updateAt row newLineFrom $ field
    in  newField

-- Функция для обновления столбца игрового поля
updateCol :: GameElement -> GameElement -> GameElement -> (ElementPosition, ElementPosition) -> Field -> Field
updateCol gameElem oldElement newElement (oldPos@(row, col), newPos@(newRow, newCol)) field =
    let oldLine         = field !! row
        newLine = updateAt newCol (if isGoal newElement then BoxOnGoal else gameElem) . updateAt col (if isGoal oldElement then Goal else Empty) $ oldLine  -- Восстановление старого элемента на старой позиции и установка нового элемента на новой позиции
        newField = updateAt row newLine field
    in  newField

-- Функция для обновления конкретного элемента на игровом поле 
updateAt :: Int -> a -> [a] -> [a]
updateAt = go 0 where
    go :: Int -> Int -> a -> [a] -> [a]
    go _ _ _ []     = []
    go m n y (x:xs) = if m == n then y : xs else x : go (m + 1) n y xs
