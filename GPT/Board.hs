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

nuevoTablero :: Tablero
nuevoTablero = replicate 7 (replicate 6 0)

reemplazar :: Int -> a -> [a] -> [a]
reemplazar 0 e (_:xs) = e : xs
reemplazar n e (x:xs) = x : reemplazar (n-1) e xs
reemplazar _ _ []     = []

localizarCoord :: Int -> Int -> Int -> Tablero -> Tablero
localizarCoord i 0 y (l:ls) = reemplazar y i l : ls
localizarCoord i x y (l:ls) = l : localizarCoord i (x-1) y ls
localizarCoord _ _ _ []     = []

addEnColumna :: Int -> [Int] -> [Int]
addEnColumna i [] = []
addEnColumna i (x:xs)
  | x == 0    = i:xs
  | otherwise = x : addEnColumna i xs

poner :: Int -> Int -> Tablero -> Tablero
poner _ _ [] = []
poner i 0 (x:xs)
  | lineaCompleta x = x:xs
  | otherwise    = addEnColumna i x : xs
poner i c (x:xs) = x : poner i (c-1) xs

lineaCompleta :: [Int] -> Bool
lineaCompleta = all (/= 0)

tableroCompleto :: Tablero -> Bool
tableroCompleto = all lineaCompleta

transponer :: Tablero -> Tablero
transponer ([]:_) = []
transponer x      = map head x : transponer (map tail x)

getDiagonales :: Tablero -> Tablero
getDiagonales []      = []
getDiagonales ([]:xs) = xs
getDiagonales xs      = zipWith (++) (map ((:[]) . head) xs ++ repeat [])
                                  ([] : getDiagonales (map tail xs))

victoriaVertical :: Tablero -> Bool
victoriaVertical = any aux
  where
    aux (x1:x2:x3:x4:xs) = (x1 /= 0 && x1 == x2 && x2 == x3 && x3 == x4) || aux (x2:x3:x4:xs)
    aux _                = False

victoriaHorizontal :: Tablero -> Bool
victoriaHorizontal = victoriaVertical . transponer

victoriaDiagonal :: Tablero -> Bool
victoriaDiagonal = victoriaVertical . getDiagonales

victoriaDiagonal2 :: Tablero -> Bool
victoriaDiagonal2 = victoriaVertical . getDiagonales . reverse

haGanado :: Tablero -> Bool
haGanado b = victoriaVertical b || victoriaHorizontal b || victoriaDiagonal b || victoriaDiagonal2 b

haFinalizado :: Tablero -> Bool
haFinalizado b = haGanado b || tableroCompleto b
