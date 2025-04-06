valorAbsoluto :: Float -> Float 
valorAbsoluto x | x > 0 = x
                | otherwise = -x

bisiesto :: Int -> Bool 
bisiesto x | mod x 4 == 0 = True
           | otherwise = False

factorial :: Int -> Int 
factorial 1 = 1
factorial x  = x * factorial(x-1)

esPrimo :: Int -> Bool
esPrimo 2 = True
esPrimo x = menorDivisor x == x

menorDivisor :: Int -> Int 
menorDivisor 0 = 0
menorDivisor x | x == 1 = 1
               | otherwise = menorDivisorDesde x 1

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde 1 y = 1
menorDivisorDesde x y | mod x y == 0  && y /= 1 = y
                      | otherwise = menorDivisorDesde x (y+1)

cantDivisoresPrimos :: Int -> Int 
cantDivisoresPrimos x = length $ filter (\d -> esPrimo d && mod x d == 0)[2..x] 

inverso :: Float -> Maybe Float -- Maybe Float toma Just Float (es un Float normal) o Nothing para prevenir errores y casos excepcionales
inverso x | x > 0 = Just (1 / x)
          | otherwise = Nothing 

aEntero :: Either Int Bool -> Int -- La unica forma de usar el tipo Either Int Bool es haciendo pattern matching
aEntero (Left x) = x
aEntero (Right True) = 1
aEntero (Right False) = 0 

divisores :: Int -> [Int]
divisores x = filter (\d -> mod x d == 0)[2..x]

limpiar :: String -> String -> String
limpiar x y = [d | d <- y, notElem d x]

caracteresEnComun :: String -> String -> String --Es la invesa a limpiar
caracteresEnComun x y = [d | d <- x, elem d y]

promedio :: [Float] -> Float
promedio x = sum x / fromIntegral (length x) 

difPromedio :: [Float] -> [Float]
difPromedio x = [d - promedio x | d <- x]

difPromedio2Aux :: [Float] -> Float -> [Float]
difPromedio2Aux [] _ = []
difPromedio2Aux (x:xs) promedio = (x - promedio) : difPromedio2Aux (xs) promedio 

difPromedio2 :: [Float] -> [Float] -- difPromedio usando recursion
difPromedio2 [] = []
difPromedio2 x = difPromedio2Aux x (promedio x)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x == y && todosIguales(y:xs) = True
                      | otherwise = False

sumaN :: Int -> [Int] -> [Int]
sumaN k xs = [d + k | d <- xs]

aparece :: Int -> [Int] -> Bool
aparece _ [] = False
aparece y (x:xs) | y == x = True
                 | otherwise = aparece y xs 

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs) 

insertar :: Int -> [Int] -> [Int]
insertar x [] = [x]
insertar x (y:ys) | x > y = y : x : ys 
                  | otherwise = y : insertar x ys 

data Direccion = Norte | Este | Sur | Oeste 
opuesta :: Direccion -> Direccion
opuesta Norte = Sur
opuesta Sur = Norte
opuesta Este = Oeste
opuesta Oeste = Este

--data Maybe a = Nothing | Just a
data AB a = Nil | Bin (AB a) a (AB a) -- modelo de arbol binario dado en la guia

buscar :: Eq a => a -> AB (a, b) -> Maybe b
buscar _ Nil = Nothing
buscar k (Bin izq (a, b) der) | k == a = Just b 
                              | otherwise = case buscar k izq of 
                                    Just valor -> Just valor -- Si encuentra a k, devuelve su valor asociado 
                                    Nothing -> buscar k der  -- Si no encuentra nada, que vaya por la rama derecha

buscar2 :: Eq a => a -> AB (a, b) -> Maybe b
buscar2 _ Nil = Nothing
buscar2 k (Bin izq (a, b) der) | k == a = Just b
                               | isNothing (buscar2 k izq) = buscar2 k der
                               | otherwise = buscar2 k izq

ordenTotal :: Ord a => a -> a -> Bool
ordenTotal x y | x > y = True
               | otherwise = False

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f x [] = x
merge f [] x = x
merge f (x:xs) (y:ys) | f x y = x : merge f xs (y:ys)
                      | otherwise = y : merge f (x:xs) ys

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort f (x:xs) = merge f (mergeSort f left) (mergeSort f right)
    where 
        (left, right) = splitInTwo (x:xs)

splitInTwo :: [a] -> ([a],[a])
splitInTwo [] = ([], [])
splitInTwo [x] = ([x], [])
splitInTwo [x, y] = ([x], [y])
splitInTwo (x:y:xs) = (x:xs1, y: xs2)
    where 
        (xs1, xs2) = splitInTwo xs

dividir :: [a] -> ([a],[a])
dividir [] = ([],[])
dividir [x] = ([x],[])
dividir xs = (take n xs, drop n xs)
    where n = div (length xs) 2 

mergeSort2 :: (a -> a -> Bool) -> [a] -> [a]
mergeSort2 _ [] = []
mergeSort2 f xs = merge f (mergeSort2 f left) (mergeSort2 f right)
    where (left, right) = dividir xs 

vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB _ = False 

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq a der) = Bin (negacionAB izq) (not a) (negacionAB der) --Aplica la funcion de su parent recursivamente

productoAB :: AB Int -> Int 
productoAB Nil = 0
productoAB (Bin izq a der) = productoLista (inorden (Bin izq a der)) 

productoLista :: [Int] -> Int
productoLista [x] = x
productoLista (x:xs) = x * productoLista(xs)

inorden :: AB Int -> [Int]
inorden Nil = []
inorden (Bin izq a der) = (inorden izq) ++ [a] ++ (inorden der) -- No necesito armar un arbol sino una lista, asi que concateno en orden inorden (de izq a raiz a derecha)

listTimes2 :: [Int] -> [Int]
listTimes2 xs = [x * 2 | x <- xs]

listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = zipWith (+) xs ys  -- Va sumando posicion por posicion

