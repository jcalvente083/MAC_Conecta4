module Computer (jugarIA) where

import Board

secuenciaLineal :: Int -> Int -> Int -> Int -> [Int] -> Int
secuenciaLineal n 0 i _ b = 1 + secuenciaLineal n n i 0 b
secuenciaLineal _ _ _ _ [] = 0
secuenciaLineal n k i z (x:xs)
  | x == i && x == z = secuenciaLineal n (k-1) i x xs
  | x == i && k == n = secuenciaLineal n (k-1) i x xs
  | otherwise        = secuenciaLineal n n i x xs

secuenciaVertical :: Int -> Int -> Tablero -> Int
secuenciaVertical n i = sum . map (secuenciaLineal n n i 0)

secuenciaHorizontal :: Int -> Int -> Tablero -> Int
secuenciaHorizontal n i = secuenciaVertical n i . transponer

secuenciaDiagonal1 :: Int -> Int -> Tablero -> Int
secuenciaDiagonal1 n i = secuenciaVertical n i . getDiagonales

secuenciaDiagonal2 :: Int -> Int -> Tablero -> Int
secuenciaDiagonal2 n i = secuenciaVertical n i . getDiagonales . reverse

secuencia :: Int -> Int -> Tablero -> Int
secuencia n i b = secuenciaVertical n i b +
                secuenciaHorizontal n i b +
                secuenciaDiagonal1 n i b +
                secuenciaDiagonal2 n i b

evaluar :: Tablero -> Int
evaluar b = 100 * secuencia 4 2 b + 5 * secuencia 3 2 b + 2 * secuencia 2 2 b
       - 1000 * secuencia 4 1 b - 5 * secuencia 3 1 b - 2 * secuencia 2 1 b

data Arbol a = Nodo a [Arbol a]
  deriving Show

generalArbol :: Int -> Int -> Tablero -> Arbol Tablero
generalArbol i 0 b = Nodo b []
generalArbol i d b = Nodo b [generalArbol (3-i) (d-1) (poner i x b) | x <- [0..6], not $ lineaCompleta (b !! x)]

obtenerValor :: Arbol a -> a
obtenerValor (Nodo a _) = a

obtenerSubNodos :: Arbol a -> [Arbol a]
obtenerSubNodos (Nodo _ a) = a

obtenerMaxValorArbol :: [Arbol Int] -> Int
obtenerMaxValorArbol = maximum . map obtenerValor

obtenerMinValorArbol :: [Arbol Int] -> Int
obtenerMinValorArbol = minimum . map obtenerValor

obtenerCoeficientes :: Int -> Arbol Tablero -> Arbol Int
obtenerCoeficientes i (Nodo b []) = Nodo (evaluar b) []
obtenerCoeficientes i (Nodo b ts) = Nodo (f x) x
  where
    n = 3 - i
    f = if i == 1 then obtenerMinValorArbol else obtenerMaxValorArbol
    x = [obtenerCoeficientes n t | t <- ts]

contarPiezasJug1 :: Tablero -> Int
contarPiezasJug1 = sum . map (length . filter (== 1))

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
