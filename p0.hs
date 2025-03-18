import GHC.Base (BCO)
import System.Win32 (xBUTTON1)
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

difPromedio2 :: [Float] -> [Float]
difPromedio2 [] = []
difPromedio2 x = difPromedio2Aux x (promedio x)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x == y && todosIguales(y:xs) = True
                      | otherwise = False

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq a der) = Bin (negacionAB izq) (not a) (negacionAB der)

productoAB :: AB Int -> Int 
productoAB Nil = 0
productoAB (Bin izq a der) = productoLista (inorden (Bin izq a der)) 

productoLista :: [Int] -> Int
productoLista [x] = x
productoLista (x:xs) = x * productoLista(xs)

inorden :: AB Int -> [Int]
inorden Nil = []
inorden (Bin izq a der) = (inorden izq) ++ [a] ++ (inorden der)

