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