module Computer (playComputer) where

import Board 

-- |Cuenta secuencias consecutivas de n piezas del jugador i
lineStreak :: Int -> Int -> Int -> Int -> [Int] -> Int
lineStreak n 0 i _ b = 1 + lineStreak n n i 0 b
lineStreak _ _ _ _ [] = 0
lineStreak n k i z (x:xs)
  | x == i && x == z = lineStreak n (k-1) i x xs
  | x == i && k == n = lineStreak n (k-1) i x xs
  | otherwise        = lineStreak n n i x xs

-- |Cuenta secuencias verticales
verticalStreak :: Int -> Int -> [[Int]] -> Int
verticalStreak n i = sum . map (lineStreak n n i 0)

-- |Cuenta secuencias horizontales
horizontalStreak :: Int -> Int -> [[Int]] -> Int
horizontalStreak n i = verticalStreak n i . transpose

-- |Cuenta secuencias diagonales (de arriba izquierda a abajo derecha)
diag1Streak :: Int -> Int -> [[Int]] -> Int
diag1Streak n i = verticalStreak n i . diagonals

-- |Cuenta secuencias diagonales (de abajo izquierda a arriba derecha)
diag2Streak :: Int -> Int -> [[Int]] -> Int
diag2Streak n i = verticalStreak n i . diagonals . reverse

-- |Cuenta todas las secuencias de longitud n del jugador i
streaks :: Int -> Int -> [[Int]] -> Int
streaks n i b = verticalStreak n i b +
                horizontalStreak n i b +
                diag1Streak n i b +
                diag2Streak n i b

-- |Función de evaluación para el jugador 2 (IA)
eval :: [[Int]] -> Int
eval b = 100 * streaks 4 2 b + 5 * streaks 3 2 b + 2 * streaks 2 2 b
       - 1000 * streaks 4 1 b - 5 * streaks 3 1 b - 2 * streaks 2 1 b

-- |Árbol de movimientos
data Tree a = Node a [Tree a]
  deriving Show

-- |Genera el árbol de movimientos posibles para el jugador i
genTree :: Int -> Int -> [[Int]] -> Tree [[Int]]
genTree i 0 b = Node b []
genTree i d b = Node b [genTree (3-i) (d-1) (place i x b) | x <- [0..6], not $ isLineFull (b !! x)]

-- |Obtiene el valor de un nodo
getVal :: Tree a -> a
getVal (Node a _) = a

-- |Obtiene los subnodos de un nodo
getSubNodes :: Tree a -> [Tree a]
getSubNodes (Node _ a) = a

-- |Encuentra el máximo valor en un árbol de enteros
maxTree :: [Tree Int] -> Int
maxTree = maximum . map getVal

-- |Encuentra el mínimo valor en un árbol de enteros
minTree :: [Tree Int] -> Int
minTree = minimum . map getVal

-- |Genera coeficientes para cada movimiento posible
genCoef :: Int -> Tree [[Int]] -> Tree Int
genCoef i (Node b []) = Node (eval b) []
genCoef i (Node b ts) = Node (f x) x
  where
    n = 3 - i
    f = if i == 1 then minTree else maxTree
    x = [genCoef n t | t <- ts]

-- |Cuenta todas las piezas del jugador 1
count1s :: [[Int]] -> Int
count1s = sum . map (length . filter (== 1))

-- |Juega el movimiento del bot con profundidad d
playComputer :: Int -> [[Int]] -> [[Int]]
playComputer d b
  | count1s b == 1 = place 2 3 b -- Heurística inicial
  | otherwise      = pos
    where
      mt = genTree 2 d b
      ct = genCoef 2 mt
      ts = getSubNodes mt
      tsc = getSubNodes ct
      mct = maxTree tsc
      pos = head [getVal (ts !! x) | x <- [0..length ts - 1], getVal (tsc !! x) == mct]
