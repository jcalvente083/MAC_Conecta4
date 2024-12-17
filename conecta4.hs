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
    
-- Ruta del archivo estática
filePath :: FilePath
filePath = "board_state.txt"

-- Funcion para crear un archivo con un tablero vacío
createEmptyBoard :: IO ()
createEmptyBoard =  writeBoard nuevoTablero

-- Función para leer el estado del tablero desde un archivo
readBoard :: IO Tablero
readBoard = do
    contents <- readFile filePath
    let board = map (map read . words) (lines contents)
    return board

-- Función para escribir el estado del tablero en un archivo
writeBoard :: Tablero -> IO ()
writeBoard board = do
    let contents = unlines (map (unwords . map show) board)
    writeFile filePath contents

-- Función para esperar hasta que el archivo haya cambiado
waitForFileChange :: UTCTime -> IO ()
waitForFileChange lastModTime = do
    threadDelay 1000000  -- Esperar 1 segundo (1000000 microsegundos)
    newModTime <- getModificationTime filePath
    if newModTime /= lastModTime
        then return ()
        else waitForFileChange lastModTime

