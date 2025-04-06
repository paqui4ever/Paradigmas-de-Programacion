triple :: Float -> Float 
triple = (*) 3 -- triple x = (*) 3 x 

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<) 18 -- esMayorDeEdad x = 18 ´<´ x 

--(.) :: (b -> c) -> (a -> b) -> a -> c  
--(.) f g x = f (g x) 

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g = \x -> f (g x) -- lo mismo que (.) pero con una funcion lambda

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x -- es una funcion que solamente camibia el orden de los parametros de entrada

flipL :: (a -> b -> c) -> (b -> a -> c)
flipL f = \x y  -> f y x -- lo mismo que arriba pero con una lambda

const5 :: a -> Int -- funcion de prueba para la funcion del prelude const
const5 x = const 5 x 

constL :: a -> (b -> a)
constL x = \_ -> x -- dada cualquier entrada devuelve una funcion devuelve siempre lo que se dio como entrada

-- Versiones previas
maximo :: Ord a => [a] -> a 
maximo [x] = x
maximo (x:xs) = if x > maximo xs then x else maximo xs

minimo :: Ord a => [a] -> a 
minimo [x] = x
minimo (x:xs) = if x < minimo xs then x else minimo xs

listaMasCorta :: [[a]] -> [a]
listaMasCorta [] = []
listaMasCorta [xs] = xs
listaMasCorta (xs:xss) = if length xs > length (listaMasCorta xss) then xs else listaMasCorta xss

-- Voy a abstraerme mas y voy a hacer una funcion mejorSegun que toma un criterio y una lista y devuelve el que mejor lo cumpla

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun p (x:xs) = if p x (mejorSegun p xs) then x else mejorSegun p xs -- para no repetir mucho puedo guardar en una variable mejorSegun p xs

maximo2 :: Ord a => [a] -> a
maximo2 = mejorSegun (>)

minimo2 :: Ord a => [a] -> a 
minimo2 = mejorSegun (<)

listaMasCorta2 :: [[a]] -> [a]
listaMasCorta2 (xs:xss) = mejorSegun (\x y -> length x < length y) (xs:xss)

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n xs = filter (\x -> length x == n) xs 

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n = filter (\f -> f n == n)

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse (map reverse xs)

reverseAnidado2 :: [[Char]] -> [[Char]]
reverseAnidado2 = \xs -> reverse (map reverse xs)

reverseAnidado3 :: [[Char]] -> [[Char]]
reverseAnidado3 = reverse . (map reverse)

paresCuadrados :: [Int] -> [Int]
paresCuadrados xs = map (\x -> if even x then x*x else x) xs -- aprovecho la lambda para filtrar los impares y aplicar el cuadrado a los pares nomas

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' p = foldr (\x acc -> p x : acc) []

intercalar :: a -> [a] -> [a]
intercalar e = foldr (\x acc -> x : e : acc) []

concatF :: [[a]] -> [a]
concatF = foldr (++) []