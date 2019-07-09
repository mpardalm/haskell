-- Estructuras de Datos. 2o Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: Pardal Martín, Miguel
--
-- Relación de Ejercicios 1.

import Test.QuickCheck


--Ejercicio 1
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x^2 + y^2 == z^2

terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = (x^2 - y^2, 2*x*y, x^2 + y^2)

p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
    where
        (l1, l2, h) = terna x y

--Ejercicio 2
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

--Ejercicio 3
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y)   | x > y     = (y,x)
                | otherwise = (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
    where enOrden (x,y) = x<=y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
    where mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) | x > y     = ordena3(y,x,z)
                | y > z     = ordena3(x,z,y)
                | otherwise = (x,y,z)

--Ejercicio 4
max2 :: Ord a => a -> a -> a
max2 x y | x > y     = x
         |otherwise  = y

--Ejercicio 5
entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z)   | x > y && x < z    = True
                | otherwise         = False

--Ejercicio 6
iguales3 :: Eq a => (a,a,a) -> Bool
--iguales3 (x,y,z) = x == y && x == z && y == z
iguales3 (x,y,z)    | x /= y    = False
                    | x /= z    = False
                    | z /= y    = False
                    | otherwise = True

--Ejercicio 7
type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer
descomponer :: TotalSegundos -> (Horas, Minutos, Segundos)
descomponer x = (horas, minutos, segundos)
  where
    horas = div x 3600
    minutos = div (x - (horas * 3600)) 60
    segundos = (x - horas*3600 - minutos*60)

p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x && entre m (0,59) && entre s (0,59)
  where (h,m,s) = descomponer x

--Ejercicio 8
unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x * unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) == x

--Ejercicio 10
raices :: Double -> Double -> Double -> (Double,Double)
raices x y z    | y^2 - 4*x*z < 0 = error "Raíces no reales"
                | otherwise = (a,b)
                where
                    a = (-y + sqrt (y^2 - 4*x*z)) / 2*x
                    b = (-y - sqrt (y^2 - 4*x*z)) / 2*x

--Ejercicio 11
esMultiplpo :: Integer -> Integer -> Bool
esMultiplpo x y | mod x y == 0  =True
                | otherwise     = False
                
--Ejercicio 12
-- (==>>) :: Bool -> Bool -> Bool


--Ejercicio 13
-- esBisiesto :: Integer -> Bool
-- esBisiesto x    | mod x 4 == 0 && (mod x 100 == 0 ==> mod x 400 == 0) = True
--                 | otherwise                         = False

--Ejercicio 14
potencia :: Integer -> Integer -> Integer
potencia x y    | y == 0 = 1
                | y > 0 = x * potencia x (y-1)
                | otherwise = error "Exponente Negativo"

potencia' :: Integer -> Integer -> Integer
potencia' x y   | y == 0 = 1
                | even y = potencia' x (div y 2) * potencia' x (div y 2)
                | otherwise = x * potencia' x (div (y-1) 2) * potencia' x (div (y-1) 2)

--Ejercicio 15
factorial :: Integer -> Integer
factorial x | x == 1 || x == 0 = 1
            | x > 0 = x * factorial (x-1)

--Ejercicio 16
divideA :: Integer -> Integer -> Bool
divideA x y | mod y x == 0 = True
            | otherwise = False

--Ejercicio 17
mediana :: Ord a => (a,a,a,a,a) -> a
mediana (x,y,z,t,u) | x > z = mediana (z,y,x,t,u)
                    | y > z = mediana (x,z,y,t,u)
                    | t < z = mediana (x,y,t,z,u)
                    | u < z = mediana (x,y,u,t,z)
                    | otherwise = z