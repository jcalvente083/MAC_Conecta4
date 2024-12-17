import Board
import Computer 
import Conecta4
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Monad.RWS (MonadState(put))

main :: IO ()
main = do
    lastModTime <- getModificationTime filePath

    -- Espera hasta que el archivo haya cambiado
    putStrLn "Esperando cambio en el archivo..."
    waitForFileChange lastModTime

    -- Leer tablero actual
    boardActual <- readBoard
    putStrLn "Tablero leido"

    if haFinalizado boardActual
        then return ()
        else do
            -- Jugar IA

            putStrLn "Jugando IA..."
            let nuevoBoard = jugarIA 3 boardActual
            putStrLn "IA ha jugado"

            -- Guardar el nuevo estado del tablero
            writeBoard nuevoBoard

            if haFinalizado nuevoBoard
                then return ()
                else do
                    main
