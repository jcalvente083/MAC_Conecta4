module Board
  ( newBoard
  , place
  , isFinished
  , transpose
  , diagonals
  , isLineFull
  ) where

-- |Generar un tablero vacío, 0 = Ninguno, 1 = Jugador 1, 2 = Jugador 2
newBoard :: [[Int]]
newBoard = replicate 7 (replicate 6 0) -- 7 columnas, 6 filas

-- |Reemplaza un elemento en una lista en la posición n
replace :: Int -> a -> [a] -> [a]
replace 0 e (_:xs) = e : xs
replace n e (x:xs) = x : replace (n-1) e xs
replace _ _ []     = []

-- |Coloca al jugador i en las coordenadas x, y del tablero
placeCoord :: Int -> Int -> Int -> [[Int]] -> [[Int]]
placeCoord i 0 y (l:ls) = replace y i l : ls
placeCoord i x y (l:ls) = l : placeCoord i (x-1) y ls
placeCoord _ _ _ []     = []

-- |Agrega al jugador i en la parte inferior de una columna
addInColumn :: Int -> [Int] -> [Int]
addInColumn i [] = [] -- Convención: No debería suceder
addInColumn i (x:xs)
  | x == 0    = i:xs
  | otherwise = x : addInColumn i xs

-- |Coloca al jugador i en una columna específica del tablero
place :: Int -> Int -> [[Int]] -> [[Int]]
place _ _ [] = [] -- Convención
place i 0 (x:xs)
  | isLineFull x = x:xs
  | otherwise    = addInColumn i x : xs
place i c (x:xs) = x : place i (c-1) xs

-- |Verifica si una línea (columna) está llena
isLineFull :: [Int] -> Bool
isLineFull = all (/= 0)

-- |Verifica si el tablero está completamente lleno
isBoardFull :: [[Int]] -> Bool
isBoardFull = all isLineFull

-- |Transpone una matriz
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)

-- |Obtiene todas las diagonales de una matriz
diagonals :: [[a]] -> [[a]]
diagonals []      = []
diagonals ([]:xs) = xs
diagonals xs      = zipWith (++) (map ((:[]) . head) xs ++ repeat [])
                                  ([] : diagonals (map tail xs))

-- |Verifica si hay una victoria vertical
verticalWin :: [[Int]] -> Bool
verticalWin = any aux
  where
    aux (x1:x2:x3:x4:xs) = (x1 /= 0 && x1 == x2 && x2 == x3 && x3 == x4) || aux (x2:x3:x4:xs)
    aux _                = False

-- |Verifica si hay una victoria horizontal
horizontalWin :: [[Int]] -> Bool
horizontalWin = verticalWin . transpose

-- |Verifica si hay una victoria en diagonal (de arriba izquierda a abajo derecha)
diag1Win :: [[Int]] -> Bool
diag1Win = verticalWin . diagonals

-- |Verifica si hay una victoria en diagonal (de abajo izquierda a arriba derecha)
diag2Win :: [[Int]] -> Bool
diag2Win = verticalWin . diagonals . reverse

-- |Verifica si hay alguna condición de victoria
isWin :: [[Int]] -> Bool
isWin b = verticalWin b || horizontalWin b || diag1Win b || diag2Win b

-- |Verifica si el juego ha terminado
isFinished :: [[Int]] -> Bool
isFinished b = isWin b || isBoardFull b