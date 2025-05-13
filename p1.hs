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

-- Antes del parcial

take1 :: Int -> [a] -> [a]
take1 n = reverse . foldl (\acc x -> if n == length acc then acc else x:acc) []

take2 :: Int -> [a] -> [a]
take2 n = reverse . foldr (\x acc -> if n == length acc then acc else acc ++ [x]) []

take'' :: [Int] -> Int -> [Int]
take'' = foldr (\x acc -> \n -> if n==0 then [] else x : acc (n-1)) (const [])

data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b 
foldAEB cHoja cBin (Hoja e) = cHoja e -- Si nos encontramos una hoja aplicamos la funcion a esa hoja
foldAEB cHoja cBin (Bin izq x der) = cBin (acc izq) x (acc der) -- Si nos encontramos un subarbol, aplicamos el caso Bin con 
                                                                -- el acumulador respectivo para los subarboles
    where acc = foldAEB cHoja cBin 

recAEB2 :: (a -> b) -> (AEB a -> b -> a -> AEB a -> b -> b) -> AEB a -> b 
                    -- b -> a -> b -> b de normal, con estructural
recAEB2 cHoja cBin t = case t of 
    Hoja a -> cHoja a 
    Bin i r d -> cBin i (recAEB2 cHoja cBin i) r d (recAEB2 cHoja cBin d)


repetidosAEB :: Eq a => AEB a -> [a]
repetidosAEB = recAEB2 (const [])(\i recI r d recD -> if elemAEB r i || elemAEB r d then r : recI ++ recD else recI ++ recD)

elemAEB :: Eq a => a -> AEB a -> Bool
elemAEB e = foldAEB (\x -> x == e) (\recI r recD -> r == e || recI || recD) 

esABB :: Ord a => AEB a -> Bool
esABB = recAEB2 (const True)(\i recI r d recD -> if head(inorder i) > r || head(inorder d) > r then False else recI || recD)

inorder :: AEB a -> [a]
inorder = foldAEB (\x -> [x]) (\recI r recD -> recI ++ [r] ++ recD)

data Buffer a = Empty | Write Int a (Buffer a)| Read Int (Buffer a)

foldBuffer :: b -> (Int -> a -> b -> b) -> (Int -> b -> b) -> Buffer a -> b 
foldBuffer cEmpty cWrite cRead t = case t of 
    Empty -> cEmpty
    Write n x b -> cWrite n x (foldBuffer cEmpty cWrite cRead b)
    Read n b -> cRead n (foldBuffer cEmpty cWrite cRead b)

recBuffer :: b -> (Int -> a -> Buffer a -> b -> b) -> (Int -> Buffer a -> b -> b) -> Buffer a -> b 
recBuffer cEmpty cWrite cRead t = case t of 
    Empty -> cEmpty
    Write n x b -> cWrite n x b (recBuffer cEmpty cWrite cRead b)
    Read n b -> cRead n b (recBuffer cEmpty cWrite cRead b)

posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = recBuffer [] (\pos elemento buffer recB-> if notElem pos recB then pos : recB else recB) (\pos buffer recB -> removeLista pos recB)

removeLista :: Eq a => a -> [a] -> [a]
removeLista e = recr (\x xs acc -> if x == e then xs else x:xs) []

contenido :: Int -> Buffer a -> Maybe a 
contenido n = recBuffer Nothing (\pos elemento buffer recB -> if pos == n then Just elemento else recB) (\pos buffer recB -> Nothing)

contenido' :: Int -> Buffer a -> Maybe a 
contenido' n = foldBuffer Nothing (\pos elemento recB -> if pos == n then Just elemento else recB) (\pos recB -> Nothing) 

puedeCompletarLecturas :: Buffer a -> Bool 
puedeCompletarLecturas = recBuffer False (\pos elemento buffer recB -> True) (\pos buffer recB -> recB && elem pos (posicionesOcupadas buffer))

deshacer :: Buffer a -> Int -> Buffer a 
deshacer = recBuffer (const Empty) (\pos elemento buffer recB -> \e -> if e == 0 then Write pos elemento buffer else recB (e - 1))(\pos buffer recB -> \e -> if e == 0 then Read pos buffer else recB (e-1))
-- Tomo un e que es mi numero de pasos que quiero retroceder y voy yendo para atras aprovechandome de aplicaciones parciales con recB (una funcion de Int -> Buffer)

data AT a = NilT | Tri a (AT a) (AT a) (AT a)

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b 
foldAT cNil cTri t = case t of
    NilT -> cNil
    Tri x ri rm rd -> cTri x (acc ri) (acc rm) (acc rd)

    where acc = foldAT cNil cTri 

preorder :: AT a -> [a]
preorder = foldAT [] (\x ri rm rd -> [x] ++ ri ++ rm ++ rd)

mapAT :: (a -> b) -> AT a -> AT b 
mapAT f = foldAT NilT (\x ri rm rd -> Tri (f x) ri rm rd)

nivel :: AT a -> Int -> [a]
nivel = foldAT (const []) (\x ri rm rd -> \e -> if e == 0 then [x] else ri (e-1) ++ rm (e-1) ++ rd (e-1)) 
-- cuando llegamos a e-1 == 0 es porque estamos en el nivel deseado (porque vamos de derecha a izq) y por tanto devolvemos los [x] de ese nivel

data Cola a = VaciaT | Cons a (Cola a) 

proximoC :: Cola a -> a 
proximoC VaciaT = undefined
proximoC (Cons x VaciaT) = x 
proximoC (Cons _ queue) = proximoC queue 

foldCola :: b -> (a -> b -> b) -> Cola a -> b 
foldCola cVacia cCola q = case q of 
    VaciaT -> cVacia
    Cons x queue -> cCola x (acc queue)

    where acc = foldCola cVacia cCola 

lengthCola :: Cola a -> Int 
lengthCola = foldCola 0 (\x recQ -> 1 + recQ)

foldlCola :: (b -> a -> b) -> b -> Cola a -> b 
foldlCola cCola cVacia q = case q of -- es como foldlCola f acc q donde f es cCola y acc es cVacia
    VaciaT -> cVacia
    Cons x queue -> foldlCola cCola (cCola cVacia x) queue

proximoCF :: Cola a -> Maybe a 
proximoCF = foldlCola (\recQ x -> Just x) Nothing 

desencolar :: Cola a -> Cola a
desencolar = foldCola VaciaT (\x recQ -> case recQ of 
                                        VaciaT -> VaciaT -- Como llegamos al primero agregado, no lo volvemos a agregar
                                        _ -> Cons x recQ) -- Vamos agregando todos

desencolarR :: Cola a -> Cola a
desencolarR VaciaT = VaciaT
desencolarR (Cons x queue) = Cons x (desencolarR queue)
desencolarR (Cons _ VaciaT) = VaciaT