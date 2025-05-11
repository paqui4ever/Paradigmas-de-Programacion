curry :: ((a, a) -> b) -> (a -> a -> b)
curry f = \x -> (\y -> f(x,y)) -- \x es la funcion que toma el x, \y la que toma el y para finalmente pasarle tanto x como y a f(x,y)

curry2 :: ((a, b) -> c) -> (a -> b -> c)
curry2 f x y = f (x,y) -- como haskell es asociativo a izq curry2 f y ya currificó la funcion, la funcion puede tomar x e y separadas  

curry3 :: ((a, b) -> c) -> (a -> b -> c)
curry3 f = \x y -> f(x,y) -- es lo mismo que curry pero con sintaxis distinta

uncurry :: (a -> a -> b) -> (a,a) -> b
uncurry f = \(x,y) -> f x y -- toma una funcion currificada y devuelve una funcion que toma una tupla y la evalua por separado en f

uncurry2 :: (a -> b -> c) -> (a, b) -> c 
uncurry2 f (x,y) = f x y -- lo mismo que uncurry pero sin usar una funcion lambda

sumaF :: [Int] -> Int
sumaF [] = 0
sumaF xs = foldr (+) 0 xs

elemF :: Ord a => a -> [a] -> Bool
elemF _ [] = False
elemF x xs = foldr (\d acc -> x==d || acc) False xs -- Setteo mi acumulador inicial en False y si se cumple que x == d entonces tendré un True como resultado

concatF :: [a] -> [a] -> [a]
concatF xs [] = xs
concatF [] ys = ys
concatF xs ys = foldr (:) ys xs -- Le agrego de derecha a izquierda los elementos de xs al frente de ys

filterF :: (a -> Bool) -> [a] -> [a]
filterF _ [] = []
filterF f xs = foldr (\d acc -> if f d then d : acc else acc) [] xs  

filterF2 :: (a -> Bool) -> [a] -> [a]
filterF2 _ [] = []
filterF2 f xs = foldr (\d acc -> [d | f d] ++ acc) [] xs -- Si f d vale entonces devolve [] y concatenalo con el acumulador

mapF :: (a -> b) -> [a] -> [b]
mapF _ [] = []
mapF f xs = foldr (\d acc -> f d : acc) [] xs -- A cada elemento de la lista le aplico f y lo concateno con el acumulador 

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun p (x:xs) = if p x (mejorSegun p xs) then x else mejorSegun p xs -- para no repetir mucho puedo guardar en una variable mejorSegun p xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales [x] = [x]
sumasParciales (x:y:xs) = x : sumasParciales (x + y : xs) 

sumasParciales2 :: Num a => [a] -> [a]
sumasParciales2 = tail. reverse . foldl (\acc x -> (x + head acc) : acc) [0]  

sumaAlt :: Num a => [a] -> a
sumaAlt [x] = x
sumaAlt (x:y:xs) = (x - y) + sumaAlt (xs) 

sumaAlt2 :: Num a => [a] -> a 
sumaAlt2 = foldr (\x acc -> x - acc) 0 -- ej: [1,2,3] es sumaAlt2 [1,2,3] = (1 - (2 - (3 - 0))) = 1 - (2 - 3) = 1 - (-1) = 2

sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (\acc x -> x - acc) 0 

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones [x,y] = [[x,y], [y,x]]
permutaciones (x:y:xs) = [x,y] : permutaciones (y:xs) ++ permutaciones (x:xs)

insertar :: a -> Int -> [a] -> [a]
insertar e index = snd . foldr (\x (pos, list) -> if pos == index then (pos + 1, e : x : list) else (pos + 1, x : list)) (0, [])

permutaciones' :: [a] -> [[a]]
permutaciones' = snd . recr (\x xs (pos, res) -> (pos+1, insertar xs pos res)) (0, [])

prefijos :: [a] -> [[a]]
prefijos [] = []
prefijos [x] = [[x]]
prefijos (x:xs) = [x] : map (x:) (prefijos (xs)) 

prefijos2 :: [a] -> [[a]]
prefijos2 = foldr (\x acc -> [x] : map (x:) acc) [] -- con el map agrego el siguiente prefijo a mi acumulador

-- prefijos [5,1,2] = [] : ([5] : ([5,1] : ([5,1,2])))

sublistas :: [a] -> [[a]]
sublistas = foldr (\x acc -> acc ++ map (x:) acc ++ [[x]]) [[]] -- a todas las listas sublistas que ya tengo les agrego x adelante y ademas x como elemento individual

partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x:) acc) [[]]

--sublistas2 :: [a] -> [[a]]
--sublistas2 [] = []
--sublistas2 [x] = [[x]]
--sublistas2 (x:xs) = [x] :  

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares = snd . foldr (\x (b, xs) -> if b then (False, x : xs) else (True, xs)) (True, [])  -- Uso una tupla que va alternando de valor booleano para inicar si es posicion par o no y devuelvo solo los 
                                                                                                               -- segundos elementos de la tupla 
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs acc -> if x == e then xs else x : acc) []

--sacarUna2 :: Eq a => a -> [a] -> [a] -> [a]
--sacarUna2 _ _ [] = []
--sacarUna2 e ys (x:xs) = if e == x then ys ++ xs else sacarUna2 e ys xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs acc -> if elem < x then elem : x : xs else x : acc) [] -- Conviene insertar recien si el elemento a agregar es mas chico que el que estoy viendo en ese momento
                    -- si lo hago viendo si elem es mas grande que el x de ese momento puede que el de la posicion siguiente a ese x tambien sea mas chico que elem, y queda desordenada la lista 

mapPares :: (a -> a -> a) -> [(a,a)] -> [a]
mapPares f = map (\(x,y) -> uncurry2 f (x,y))

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x acc ys -> (x, head ys) : acc (tail ys) ) (const []) -- Me armo la lista con acc

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x acc ys -> (f x (head ys)) : acc (tail ys)) (const []) -- Lo mismo que para armarPares

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+)) -- zipWith (+) suma dos filas elemento a elemento, como lo quiero hacer fila a fila, hago otro zipWith

--trasponer :: [[Int]] -> [[Int]]
--trasponer = 