module Computer (jugarIA) where

import Board

-- | Cuenta secuencias consecutivas de n piezas del jugador i.
--   n: Longitud de la secuencia objetivo.
--   k: Contador descendente para validar la secuencia.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   z: Ultimo valor analizado en la secuencia (para verificar continuidad).
--   b: Lista de enteros representando el tablero.
-- Devuelve el numero de elementos de la secuencia.
secuenciaLineal :: Int -> Int -> Int -> Int -> [Int] -> Int
secuenciaLineal n 0 i _ b = 1 + secuenciaLineal n n i 0 b
secuenciaLineal _ _ _ _ [] = 0
secuenciaLineal n k i z (x:xs)
  | x == i && x == z = secuenciaLineal n (k-1) i x xs
  | x == i && k == n = secuenciaLineal n (k-1) i x xs
  | otherwise        = secuenciaLineal n n i x xs

-- | Cuenta secuencias verticales.
--   n: Longitud de la secuencia objetivo.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   b: Tablero representado como una lista de columnas.
secuenciaVertical :: Int -> Int -> Tablero -> Int
secuenciaVertical n i = sum . map (secuenciaLineal n n i 0)

-- | Cuenta secuencias horizontales.
--   n: Longitud de la secuencia objetivo.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   b: Tablero representado como una lista de columnas.
secuenciaHorizontal :: Int -> Int -> Tablero -> Int
secuenciaHorizontal n i = secuenciaVertical n i . transponer

-- | Cuenta secuencias diagonales (de arriba izquierda a abajo derecha).
--   n: Longitud de la secuencia objetivo.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   b: Tablero representado como una lista de columnas.
secuenciaDiagonal1 :: Int -> Int -> Tablero -> Int
secuenciaDiagonal1 n i = secuenciaVertical n i . getDiagonales

-- | Cuenta secuencias diagonales (de abajo izquierda a arriba derecha).
--   n: Longitud de la secuencia objetivo.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   b: Tablero representado como una lista de columnas.
secuenciaDiagonal2 :: Int -> Int -> Tablero -> Int
secuenciaDiagonal2 n i = secuenciaVertical n i . getDiagonales . reverse

-- | Cuenta todas las secuencias de longitud n del jugador i en el tablero.
--   n: Longitud de la secuencia objetivo.
--   i: Jugador (1 o 2) cuyo estado se analiza.
--   b: Tablero representado como una lista de columnas.
secuencia :: Int -> Int -> Tablero -> Int
secuencia n i b = secuenciaVertical n i b +
                secuenciaHorizontal n i b +
                secuenciaDiagonal1 n i b +
                secuenciaDiagonal2 n i b

-- | Evalua el estado actual del tablero desde la perspectiva de la IA.
--   b: Tablero representado como una lista de columnas.
-- Devuelve la puntuacion que indica la ventaja para la IA.
evaluar :: Tablero -> Int
evaluar b = 100 * secuencia 4 2 b + 5 * secuencia 3 2 b + 2 * secuencia 2 2 b
       - 1000 * secuencia 4 1 b - 5 * secuencia 3 1 b - 2 * secuencia 2 1 b

-- | Tipo de dato que representa un arbol de movimientos.
data Arbol a = Nodo a [Arbol a]
  deriving Show

-- | Genera el arbol de movimientos posibles para el jugador i.
--   i: Jugador actual (1 o 2).
--   d: Profundidad del arbol (numero de turnos a analizar).
--   b: Estado actual del tablero.
-- Debuelve un arbol donde cada nodo es un tablero posible y sus hijos representan movimientos futuros.
generalArbol :: Int -> Int -> Tablero -> Arbol Tablero
generalArbol i 0 b = Nodo b []
generalArbol i d b = Nodo b [generalArbol (3-i) (d-1) (poner i x b) | x <- [0..6], not $ lineaCompleta (b !! x)]

-- | Obtiene el valor de un nodo.
obtenerValor :: Arbol a -> a
obtenerValor (Nodo a _) = a

-- | Obtiene los subnodos hijos de un nodo.
obtenerSubNodos :: Arbol a -> [Arbol a]
obtenerSubNodos (Nodo _ a) = a

-- | Encuentra el maximo valor en un arbol de enteros.
obtenerMaxValorArbol :: [Arbol Int] -> Int
obtenerMaxValorArbol = maximum . map obtenerValor

-- | Encuentra el minimo valor en un arbol de enteros.
obtenerMinValorArbol :: [Arbol Int] -> Int
obtenerMinValorArbol = minimum . map obtenerValor

-- | Genera coeficientes para cada movimiento posible.
--   i: Jugador actual (1 o 2).
--   Un nodo del arbol que representa el estado del tablero.
-- Devuelve un arbol donde cada nodo contiene el puntaje del tablero.
obtenerCoeficientes :: Int -> Arbol Tablero -> Arbol Int
obtenerCoeficientes i (Nodo b []) = Nodo (evaluar b) []
obtenerCoeficientes i (Nodo b ts) = Nodo (f x) x
  where
    n = 3 - i
    f = if i == 1 then obtenerMinValorArbol else obtenerMaxValorArbol
    x = [obtenerCoeficientes n t | t <- ts]

-- | Cuenta todas las piezas del jugador 1 en el tablero.
contarPiezasJug1 :: Tablero -> Int
contarPiezasJug1 = sum . map (length . filter (== 1))

-- | Juega el movimiento del bot con profundidad d.
--   d: Profundidad del analisis del arbol (numero de turnos a predecir).
--   b: Estado actual del tablero.
-- Devuelve el nuevo estado del tablero tras el movimiento del bot.
jugarIA :: Int -> Tablero -> Tablero
jugarIA d b
  | contarPiezasJug1 b == 1 = poner 2 3 b -- Heuristica inicial
  | otherwise      = pos
    where
      mt = generalArbol 2 d b
      ct = obtenerCoeficientes 2 mt
      ts = obtenerSubNodos mt
      tsc = obtenerSubNodos ct
      mct = obtenerMaxValorArbol tsc
      pos = head [obtenerValor (ts !! x) | x <- [0..length ts - 1], obtenerValor (tsc !! x) == mct]
