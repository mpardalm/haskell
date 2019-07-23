module Bag
    ( Bag
    , empty
    , isEmpty
    , insert
    , occurrences
    , delete
    ) where

data Bag a = Empty | Node a Int (Bag a) deriving Show

empty :: Bag a
empty = Empty

isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: (Ord a) => a -> Bag a -> Bag a
insert x Empty = Node x 1 (Empty)
insert x (Node y ys s) -- x es el elemento a insertar; y es el elemento para comparar; ys número de elementos de y; s el resto de Bag
        | x < y = Node x 1 (Node y ys s)
        | x == y = Node y (ys + 1) s
        | x > y = Node y ys (insert x s)

occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty = 0
occurrences x (Node y ys s)
        | x < y = 0
        | x > y = occurrences x s
        | x == y = ys
        
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty = Empty
delete x (Node y ys s)
        | occurrences x (Node y ys s) == 0 = Node y ys s
        | x > y = Node y ys (delete x s)
        | x == y && ys == 1 = s
        | x == y = Node y (ys-1) s

union :: (Ord a) => Bag a -> Bag a -> Bag a
union x Empty = x
union Empty x = x
union (Node x xs s) (Node y ys t)
        | x < y = (Node x xs (Node y ys (union s t)))
        | x > y = (Node y ys (Node x xs (union s t)))
        | otherwise = Node x (xs + ys) (union s t)

interseccion :: (Ord a) => Bag a -> Bag a -> Bag a
interseccion x Empty = Empty
interseccion Empty x = Empty
interseccion (Node x xs s) (Node y ys t)
        | x == y = Node x (xs + ys) (interseccion s t)
        | x < y = interseccion s (Node y ys t)
        | x > y = interseccion (Node x xs s) t

diferencia :: (Ord a) => Bag a -> Bag a -> Bag a
diferencia x Empty = x
diferencia Empty x = Empty
diferencia (Node x xs s) (Node y ys t)
        | x == y = diferencia s t
        | x < y = Node x xs (diferencia s (Node y ys t))
        | x > y = diferencia (Node x xs s) t

inBag :: (Ord a) => Bag a -> Bag a -> Bool
inBag x Empty = False
inBag Empty x = True
inBag (Node x xs s) (Node y ys t)
        | x == y = inBag s t
        | x < y = False
        | x > y = inBag (Node x xs s) t