import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Concurrent (threadDelay)

-- Tipo de dato para representar el tablero
type Board = [[Int]]

-- Ruta del archivo estática
filePath :: FilePath
filePath = "board_state.txt"

-- Función para leer el estado del tablero desde un archivo
readBoard :: IO Board
readBoard = do
    contents <- readFile filePath
    let board = map (map read . words) (lines contents)
    return board

-- Función para escribir el estado del tablero en un archivo
writeBoard :: Board -> IO ()
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

-- Función para actualizar el tablero con un nuevo movimiento del jugador 2
updateBoard :: Board -> Int -> Int -> Board
updateBoard board row col = 
    take row board ++ [take col (board !! row) ++ [2] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

main :: IO ()
main = do
    -- Obtener el tiempo de última modificación del archivo
    lastModTime <- getModificationTime filePath

    -- Esperar hasta que el archivo haya cambiado
    waitForFileChange lastModTime

    -- Leer el tablero desde el archivo
    board <- readBoard
    print board
    
    threadDelay 1000000
    
    -- Actualizar el tablero con un nuevo movimiento del jugador 2 (por ejemplo, en la posición (5, 3))
    let newBoard = updateBoard board 5 3
    print newBoard

    -- Escribir el nuevo tablero en el archivo
    writeBoard newBoard
