-- Estructuras de Datos. 2o Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: Pardal Martín, Miguel
--
-- Relación de Ejercicios 2.

import Test.QuickCheck

--Ejercicio 1
data Direction = North | South | East | West deriving (Eq, Enum, Show)

instance Ord Direction where
    (<=) x y    | x << y = True
                | otherwise = False

(<<) :: Direction -> Direction -> Bool
(<<) x y    | fromEnum x < fromEnum y = True
            | otherwise = False

--Ejercicio 2
maximoYResto :: Ord a => [a] -> (a, [a])
maximoYResto [] = error "Lista Vacia"
maximoYResto (x:xs) = (maximo (x:xs), eliminarMaximo(x:xs))


maximo :: Ord a => [a] -> a
maximo [] = error "Empty List"
maximo [x] = x
maximo (x:xs) = if(x > head xs) then maximo (x: tail xs)
                else maximo xs

eliminarMaximo :: (Ord a) => [a] -> [a]
eliminarMaximo [] = error "Empty List"
eliminarMaximo (x:xs) = if (x == maximo (x:xs)) then xs
                        else x: eliminarMaximo xs

--Ejercicio 3
reparte :: [a] -> ([a], [a])
reparte [] = error "Empty List"
reparte [x] = ([x], [])
reparte (x:y:zs) = (x:l1, y:l2)
    where
        (l1,l2) = reparte zs

--Ejercicio 4
distintos :: Eq a => [a] -> Bool
distintos [] = error "Empty List"
distintos [x] = True
distintos (x:xs) = if (elem x xs) then False
                    else distintos xs

--Ejercicio 5
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y

--Ejercicio 6
divideA :: Integer -> Integer -> Bool
divideA a b 
  | a/= 0       = (mod b a == 0)
  | otherwise   = error "division por 0"

divisores :: Integer -> [Integer]
divisores a = [x | x<- [1..a], divideA x a]

--Ejercicio 7
mcd :: Integer -> Integer -> Integer
mcd a b = if (a == 0 || b == 0) then 0
          else if (a == 1 || b == 1) then 1
          else if (a == b) then a
          else if (a < 0 || b < 0) then error "Numero negativo"
          else maximum [n | n <- divisores a, divideA n b]

divisoresComunes :: Integer -> Integer -> [Integer]
divisoresComunes a b = [n | n <- divisores a, divideA n b]         

mcd' :: Integer -> Integer -> Integer
mcd' a 0 = a
mcd' a b = mcd b (mod a b)

mcm :: Integer -> Integer -> Integer
mcm a b = div (a*b) (mcd' a b)

--Ejercicio 8
esPrimo :: Integer -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo a = if(length (divisores a) == 2) then True
            else False

primosHasta :: Integer -> [Integer]
primosHasta n = [x | x <- [0..n], esPrimo x]

primosHasta' :: Integer -> [Integer]
primosHasta' n = filter(\x -> esPrimo x)[0..n]

--Ejercicio 9
pares :: Integer -> [(Integer, Integer)]
pares n = [(x,y) | x <- primosHasta (div n 2), y <- primosHasta n, x + y == n]

infix 1 ==>>
(==>>) :: Bool -> Bool -> Bool
(==>>) x y | x && not y = False
           | otherwise  = True

goldbach :: Integer -> Bool
goldbach n | n > 2 && even n ==>> length (pares n) > 0 = True
           | otherwise = False
-- goldbach n = if(n > 2 && even n && length (pares n) > 0) then True
--              else False

goldbachHasta :: Integer -> Bool
goldbachHasta n = and ([goldbach x | x <- [0..n], even x])

--Ejercicio 10
esPerfecto :: Integer -> Bool
esPerfecto n = if(sum (init (divisores n)) == n) then True
              else False

perfectosMenoresQue :: Integer -> [Integer]
perfectosMenoresQue n = [x | x <- [1..n], esPerfecto x]

--Ejercicio 11
take' :: Int -> [a] -> [a]
take' n xs = [x | (p,x) <- zip [0.. n-1] xs]

drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <- zip [0..length xs] xs, p >= n]

--Ejercicio 12
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

concat'' :: [[a]] -> [a]
concat'' (x:xs) = [n | y <- (x : xs), n <- y]

--Ejercicio 14
inserta :: (Ord a) => a -> [a] -> [a]
inserta x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs

inserta' :: (Ord a) => a -> [a] -> [a]
inserta' n [] = [n]
inserta' n (x:xs) | n < x = n : (x:xs)
                  | otherwise = x : (inserta' n xs)

ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs

--Ejercicio 15
geometrica :: Integer -> Integer -> [Integer]
geometrica n m = take 100 (iterate (*m) n)

multiplosDe :: Integer -> [Integer]
multiplosDe 0 = []
multiplosDe n = iterate (+n) n

potenciasDe :: Integer -> [Integer]
potenciasDe 0 = []
potenciasDe n = take 10 (iterate (*n) 1)

--Ejercicio 16
primeroComun :: (Ord a) => [a] -> [a] -> a
primeroComun [] [] = error "Ambas listas vacias"
primeroComun (x:xs) (y:ys) | x < y = primeroComun xs (y:ys)
                           | y < x = primeroComun (x:xs) ys
                           | otherwise = x

mcm' :: Integer -> Integer -> Integer
mcm' n m = primeroComun (multiplosDe n) (multiplosDe m)