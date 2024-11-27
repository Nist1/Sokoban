import Prelude hiding (Left, Right)
import System.Console.Terminfo
import System.IO (hFlush, stdin)
import Control.Monad (when)

import GameField
import LevelLoader
import Move

main :: IO ()
main = do
    putStrLn "Choose level 1 - 15:"
    level <- getLine
    gameField <- loadLevel (chooseLevel level)
    let goals = findGoalPositions gameField
    gameLoop level gameField goals
    

gameLoop :: FilePath -> GameField -> [ElementPosition] -> IO ()
gameLoop level gameField goals = do
    printGameField gameField
    if isLevelSolved gameField goals
        then putStrLn "Level solved successfully! Congratulations!" 
        else do
            key <- getKey
            case key of 
                "q"   -> return ()
                "ESC" -> return ()
                "r"   -> do
                    putStrLn "Restarting level..."
                    gameField <- loadLevel level
                    gameLoop level gameField goals
                let direction = case key of
                        "\ESC[A" -> Just Up
                        "\ESC[B" -> Just Down
                        "\ESC[C" -> Just Left
                        "\ESC[D" -> Just Right
                        _        -> Nothing
                case direction of
                    Just direction -> gameLoop (handleMovement direction gameField) goals
                    Nothing -> do
                        putStrLn "Invalid move. Use arrow keys."
                        gameLoop gameField goals

setupTerm :: IO ()
setupTerm = do
    setupTermFromEnv
    setEcho False
    setCbreak True

cleanupTerm :: IO ()
cleanupTerm = do
    setEcho True
    setCbreak False
    resetTerminalState

getKey :: IO String
getKey = reverse <$> getKey' ""
    where
        getKey' chars = do
            char <- getChar
            more <- hReady stdin ( if more then getKey' else return) 
            (char : chars)