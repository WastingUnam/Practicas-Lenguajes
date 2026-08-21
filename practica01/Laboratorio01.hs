module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen a b = sqrt(a^2 + b^2)


sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares (x:xs) = if (even x) then (x^2 + sumaCuadradosPares xs) else (sumaCuadradosPares xs)

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 x y =
  let media = (x + y)/2
  in ((x - media)^2 + (y - media)^2) / 2

clasificaTemperatura :: Int -> String
clasificaTemperatura a
    | a <= 0  = "frio extremo"
    | a <= 15 = "frio"
    | a <= 25 = "templado"
    | a <= 35 = "calido"
    | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala a [x] = [x]
intercala a [] = []
intercala a (x:xs) = x : a: (intercala a xs)

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
