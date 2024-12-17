import Board
import Computer 
import Conecta4
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)


main :: IO ()
main = do

    -- Obtener tiempo de última modificación del archivo
    lastModTime <- getModificationTime filePath

    -- Esperar hasta que el archivo haya cambiado
    waitForFileChange lastModTime

    -- Leer tablero actual
    boardActual <- readBoard
    
    if (haFinalizado boardActual) then return ()
    else do
        -- Juega IA
        nuevoBoard <- jugarIA 10 boardActual
        
        -- Guardar tablero
        writeBoard nuevoBoard
       
        if (haFinalizado nuevoBoard) then return ()
        else main
