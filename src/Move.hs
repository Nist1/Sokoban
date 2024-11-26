module Move where

import Prelude hiding (Left, Right)
import GameField

-- Тип для представления направления движения игрока
data MoveDirection = Up
                   | Down
                   | Left
                   | Right deriving (Eq)

-- Функция проверки на возможность сдвинуться в указанном направлении
canMoveTo :: ElementPosition -> Field -> MoveDirection -> Bool
canMoveTo (row, col) field direction =
    case field !! row !! col of
        Empty -> True
        Goal  -> True
        Box   -> moveBox field direction (row, col)
        _     -> False

-- Функция проверки на возможность свдига ящика в указанном направлении
canMoveBoxTo :: ElementPosition -> Field -> MoveDirection -> Bool
canMoveBoxTo (row, col) field direction =
    case field !! row !! col of
        Empty -> True
        Goal  -> True
        _     -> False

-- Функция для движения игрока
move :: GameField -> MoveDirection -> GameField
move gameField@(GameField field (row, col)) direction =
    let (newRow, newCol) = case direction of
            Up    -> (row - 1, col)
            Down  -> (row + 1, col)
            Left  -> (row, col - 1)
            Right -> (row, col + 1)
        newPos = (newRow, newCol)
    in if canMoveTo newPos field direction
        then updatePlayerPosition gameField newPos
        else gameField

-- Функция для движения ящика
moveBox :: GameField -> MoveDirection -> ElementPosition -> GameField
moveBox gameField@(GameField field playerPos) direction (row, col) =
    let (newRow, newCol) = case direction of
            Up    -> (row - 1, col)
            Down  -> (row + 1, col)
            Left  -> (row, col - 1)
            Right -> (row, col + 1)
        newPos = (newRow, newCol)
    in if canMoveTo newPos field direction
        then updateBoxPosition field newPos
        else gameField

-- Функция для обновления поля с новой позицией игрока
updatePlayerPosition ::
updatePlayerPosition

-- Функция для обновления поля c новой позицией ящика
updateBoxPosition :: 
updateBoxPosition
