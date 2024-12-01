module Move where

import Prelude hiding (Left, Right)
import GameField

-- Тип для представления направления движения игрока
data MoveDirection = Up
                   | Down
                   | Left
                   | Right deriving (Eq)

-- Вспомогательная функция для вычисления новой позиции элемента в соответствии с заданным направлением
changePosition :: ElementPosition -> MoveDirection -> ElementPosition
changePosition (row, col) Up    = (row - 1, col)
changePosition (row, col) Down  = (row + 1, col)
changePosition (row, col) Left  = (row, col - 1)
changePosition (row, col) Right = (row, col + 1)

-- Функция для обновления конкретного элемента на игровом поле 
updateElem :: Int -> a -> [a] -> [a]
updateElem = go 0 where
    go :: Int -> Int -> a -> [a] -> [a]
    go _ _ _ []     = []
    go m n y (x:xs) = if m == n then y : xs else x : go (m + 1) n y xs

-- Функция для обновления строки игрового поля
updateRow :: GameElement -> (ElementPosition, ElementPosition) -> Field -> Field
updateRow gameElem (oldPos@(row, col), newPos@(newRow, newCol)) field =
    let isOnGoal       = isGoal $ getElement oldPos field
        isToGoal       = isGoal $ getElement newPos field
        lineFrom       = field !! row
        lineTo         = field !! newRow
        elementOnGoal  = if gameElem == Player then PlayerOnGoal else BoxOnGoal
        newLineFrom    = updateElem col (if isOnGoal then Goal else Empty) lineFrom
        newLineTo      = updateElem newCol (if isToGoal then elementOnGoal else gameElem) lineTo
        newField       = updateElem newRow newLineTo . updateElem row newLineFrom $ field
    in  newField

-- Функция для обновления столбца игрового поля
updateCol :: GameElement -> (ElementPosition, ElementPosition) -> Field -> Field
updateCol gameElem (oldPos@(row, col), newPos@(newRow, newCol)) field =
    let isOnGoal       = isGoal $ getElement oldPos field
        isToGoal       = isGoal $ getElement newPos field
        elementOnGoal  = if gameElem == Player then PlayerOnGoal else BoxOnGoal
        oldLine        = field !! row
        newLine = updateElem newCol (if isToGoal then elementOnGoal else gameElem)
            . updateElem col (if isOnGoal then Goal else Empty) $ oldLine
        newField = updateElem row newLine field
    in  newField

-- Функция для проверки, возможно ли перемещение в заданном направлении 
canMoveTo :: MoveDirection -> GameField -> Bool
canMoveTo direction gameField =
    let field  = currentField gameField
        pos    = playerPosition gameField
        newPos = changePosition pos direction
    in  isEmpty $ getElement newPos field

-- Функция для движения игрока или игрока вместе с ящиком 
-- Позиция игрока изменяется не зависимо от того, какой элемент двигается
-- при движении ящика функция вызывается снова, чтобы сдвинуть игрока
move :: GameElement -> ElementPosition -> MoveDirection -> GameField -> GameField
move gameElem pos direction gameField =
    let curPos@(row, col)        = pos
        nextPos@(newRow, newCol) = changePosition curPos direction
        field                    = currentField gameField
        updatedField             = if col == newCol
            then updateRow gameElem (curPos, nextPos) field -- движение по горизонтали
            else updateCol gameElem (curPos, nextPos) field -- движение по вертикали
    in  GameField {currentField = updatedField, playerPosition = nextPos}

-- Функция для проверки, возможно ли толкнуть ящик в заданном направлении
canPushTo :: MoveDirection -> GameField -> Bool
canPushTo direction gameField =
    let field        = currentField gameField
        pos          = playerPosition gameField
        playerNewPos = changePosition pos direction            -- Новая позиция игрока после передвижения
        el1          = getElement playerNewPos field
        boxNewPos    = changePosition playerNewPos direction   -- Новая позиция ящика после передвижения
        el2          = getElement boxNewPos field
    in  isBox el1 && isEmpty el2

-- Функция передвижения ящика игроком
push :: MoveDirection -> GameField -> GameField
push direction gameField =
    let playerPos    = playerPosition gameField
        boxPos       = changePosition playerPos direction
        boxAfterPush = move Box boxPos direction gameField
    in  move Player playerPos direction boxAfterPush

-- Функция для обработки движения
handleMovement :: MoveDirection -> GameField -> GameField
handleMovement direction gameField
    | canMoveTo direction gameField = move Player (playerPosition gameField) direction gameField
    | canPushTo direction gameField = push direction gameField
    | otherwise                     = gameField
