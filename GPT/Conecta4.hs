module Conecta4 (
    filePath,
    readBoard,
    writeBoard,
    waitForFileChange
    ) where 

import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Concurrent (threadDelay)
import Board
    
filePath :: FilePath
filePath = "board_state.txt"

createEmptyBoard :: IO ()
createEmptyBoard = writeBoard nuevoTablero

readBoard :: IO Tablero
readBoard = do
    contents <- readFile filePath
    let board = map (map read . words) (lines contents)
    return board
    
writeBoard :: Tablero -> IO ()
writeBoard board = do

    let contents = unlines (map (unwords . map show) board)
    writeFile filePath contents

waitForFileChange :: UTCTime -> IO ()
waitForFileChange lastModTime = do
    threadDelay 1000000
    newModTime <- getModificationTime filePath
    if newModTime /= lastModTime
        then return ()
        else waitForFileChange lastModTime
