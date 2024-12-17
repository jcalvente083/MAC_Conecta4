module Board
  ( Tablero
  , nuevoTablero
  , poner
  , haFinalizado
  , transponer
  , getDiagonales
  , lineaCompleta
  ) where

type Tablero = [[Int]]

-- |Generar un tablero vacío, 0 = Ninguno, 1 = Jugador 1, 2 = Jugador 2
nuevoTablero :: Tablero
nuevoTablero = replicate 7 (replicate 6 0) -- 7 columnas, 6 filas

-- |Reemplaza un elemento en una lista en la posición n
-- | n - Indice de la lista
-- | e - Elemento a reemplazar
-- | [a] - Columna donde se reemplazará
reemplazar :: Int -> a -> [a] -> [a]
reemplazar 0 e (_:xs) = e : xs
reemplazar n e (x:xs) = x : reemplazar (n-1) e xs
reemplazar _ _ []     = []

-- |Coloca al jugador i en las coordenadas x, y del tablero
-- | i - Jugador
-- | x - Columna
-- | y - Fila
-- | Tablero
localizarCoord :: Int -> Int -> Int -> Tablero -> Tablero
localizarCoord i 0 y (l:ls) = reemplazar y i l : ls
localizarCoord i x y (l:ls) = l : localizarCoord i (x-1) y ls
localizarCoord _ _ _ []     = []

-- |Agrega al jugador i en la parte inferior de una columna
-- | i - Jugador
-- | [Int] - Columna
addEnColumna :: Int -> [Int] -> [Int]
addEnColumna i [] = [] -- Convención: No debería suceder
addEnColumna i (x:xs)
  | x == 0    = i:xs
  | otherwise = x : addEnColumna i xs

-- |Coloca al jugador i en una columna específica del tablero
-- | i - Jugador
-- | c - Columna
-- | Tablero
poner :: Int -> Int -> Tablero -> Tablero
poner _ _ [] = [] -- Convención
poner i 0 (x:xs)
  | lineaCompleta x = x:xs
  | otherwise    = addEnColumna i x : xs
poner i c (x:xs) = x : poner i (c-1) xs

-- |Verifica si una línea (columna) está llena
-- | [Int] - Columna
lineaCompleta :: [Int] -> Bool
lineaCompleta = all (/= 0)

-- |Verifica si el tablero está completamente lleno
-- | Tablero
tableroCompleto :: Tablero -> Bool
tableroCompleto = all lineaCompleta

-- |Transpone una matriz
-- | Tablero
transponer :: Tablero -> Tablero
transponer ([]:_) = []
transponer x      = map head x : transponer (map tail x)

-- |Obtiene todas las diagonales de una matriz
-- | Tablero
getDiagonales :: Tablero -> Tablero
getDiagonales []      = []
getDiagonales ([]:xs) = xs
getDiagonales xs      = zipWith (++) (map ((:[]) . head) xs ++ repeat [])
                                  ([] : getDiagonales (map tail xs))

-- |Verifica si hay una victoria vertical
-- | Tablero
victoriaVertical :: Tablero -> Bool
victoriaVertical = any aux
  where
    aux (x1:x2:x3:x4:xs) = (x1 /= 0 && x1 == x2 && x2 == x3 && x3 == x4) || aux (x2:x3:x4:xs)
    aux _                = False

-- |Verifica si hay una victoria horizontal
-- | Tablero
victoriaHorizontal :: Tablero -> Bool
victoriaHorizontal = victoriaVertical . transponer

-- |Verifica si hay una victoria en diagonal (de arriba izquierda a abajo derecha)
-- | Tablero
victoriaDiagonal :: Tablero -> Bool
victoriaDiagonal = victoriaVertical . getDiagonales

-- |Verifica si hay una victoria en diagonal (de abajo izquierda a arriba derecha)
-- | Tablero
victoriaDiagonal2 :: Tablero -> Bool
victoriaDiagonal2 = victoriaVertical . getDiagonales . reverse

-- |Verifica si hay alguna condición de victoria
-- | Tablero
haGanado :: Tablero -> Bool
haGanado b = victoriaVertical b || victoriaHorizontal b || victoriaDiagonal b || victoriaDiagonal2 b

-- |Verifica si el juego ha terminado
-- | Tablero
haFinalizado :: Tablero -> Bool
haFinalizado b = haGanado b || tableroCompleto b