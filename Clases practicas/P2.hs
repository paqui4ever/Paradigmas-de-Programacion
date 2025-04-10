dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f = foldr (\x acc -> if f x then acc else x : acc) []

sumasParciales' :: Num a => [a] -> [a]
sumasParciales' = reverse . snd . foldl (\(sum, acc) x -> (x + sum, (x+sum): acc)) (0, []) -- uso como acumulador una tupla que toma la suma total acumulada 
                                                                                           -- y despues la lista en la que voy poniendo los elementos

sumaAlt' :: Num a => [a] -> a 
sumaAlt' = foldr (\x acc -> (+) x (-acc)) 0

data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b 
foldAEB cHoja cBin (Hoja e) = cHoja e -- Si nos encontramos una hoja aplicamos la funcion a esa hoja
foldAEB cHoja cBin (Bin izq x der) = cBin (acc izq) x (acc der) -- Si nos encontramos un subarbol, aplicamos el caso Bin con 
                                                                -- el acumulador respectivo para los subarboles
    where acc = foldAEB cHoja cBin 

-- Podria escribirse de una forma mas elegante como:
foldAEB2 :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b 
foldAEB2 cHoja cBin t = case t of -- sigue siendo pattern matching pero aprovecho las funcionalidades del lenguaje
    Hoja e -> cHoja e
    Bin izq x der -> cBin (acc izq) x (acc der)

    where acc = foldAEB cHoja cBin

recAEB :: (a -> b) -> (AEB a -> b -> a -> AEB a -> b -> b) -> AEB a -> b 
recAEB cHoja cBin t = case t of
    Hoja a -> cHoja a 
    Bin izq r der -> cBin izq (recAEB cHoja cBin izq) r der (recAEB cHoja cBin der) -- Les paso ademas los subarboles izq y derechos

insertarAEB :: Ord a => a -> AEB a -> AEB a  
insertarAEB e = foldAEB (\x -> if e < x then Bin (Hoja e) x (Hoja x) else Bin (Hoja x) x (Hoja e)) (\recI r recD -> if e < r then Bin (insertarAEB e recI) r recD else Bin recI r (insertarAEB e recD))

cantHojas :: Ord a => AEB a -> Int 
cantHojas = foldAEB (const 1) (\recI r recD -> recI + recD) -- cada paso recursivo llega hasta una hoja y devuelve 1, entonces si hago la suma de las hojas, tendré el total

cantNodos :: AEB a -> Int
cantNodos = foldAEB (const 1)(\recI r recD -> recI + 1 + recD) -- por cada paso recursivo, sumo 1 y los junto 

ramas :: AEB a -> [[a]] -- devuelve todas las ramas del arbol
ramas = foldAEB (\x -> [[x]]) (\recI r recD -> (map (r:) recI) ++ (map (r:) recD)) -- Necesito mappear las raices al lado izquierdo y al lado derecho

altura :: AEB a -> Int
altura = foldAEB (const 1) (\recI _ recD -> 1 + max recI recD)

mejorSegun :: (a -> a -> Bool) -> AEB a -> a 
mejorSegun f = foldAEB () (\recI r recD -> if )

take' :: [a] -> Int -> [a]
take' [] = const []
take' (x:xs) = \n -> if n == 0 then [] else x : take' xs (n-1)

take'' :: [Int] -> Int -> [Int]
take'' = foldr (\x acc -> \n -> if n==0 then [] else x : acc (n-1)) (const [])

data Nat a = Zero | Succ (Nat a) 

foldNat :: b -> (b -> b) -> Nat a -> b -- ya procesé el Nat del Succ, entonces me quedó algo de tipo b. Como todo se construye a partir de ese Nat original y pasó a ser de tipo b entonces la funcion termina siendo de b en b
foldNat cZero cSucc n = case n of 
    Zero -> cZero
    Succ n -> cSucc (foldNat cZero cSucc n)

potencia :: Nat a -> Int 
potencia = foldNat 1 (\x -> 2 * x) 

data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a) -- los primeros dos son constructores no recursivos, los otros si

foldPoli' :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli' cX cCte cSuma cProd p = case p of 
    X -> cX 
    Cte k -> cCte k
    Suma p' q -> cSuma (foldPoli' cX cCte cSuma cProd p') (foldPoli' cX cCte cSuma cProd q)
    Prod p' q -> cProd (foldPoli' cX cCte cSuma cProd p') (foldPoli' cX cCte cSuma cProd q)


evaluar :: Num a => a -> Polinomio a -> a
evaluar e p = case p of
    x -> e
    Cte a -> a 
    Suma p' q -> (evaluar e p') + (evaluar e q) 
    Prod p' q -> (evaluar e p') * (evaluar e q)

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b -- cada uno de los parentesis y el b solo es para cada constructor
foldPol cX cCte cSuma cProd p = case p of
    X -> cX 
    Cte k -> cCte k 
    Suma p' q -> cSuma (acc p') (acc q) 
    Prod p' q -> cProd (acc p') (acc q)

    where acc = foldPol cX cCte cSuma cProd   

evaluar' :: Num a => a -> Polinomio a -> a 
evaluar' e = foldPol e id (+) (*) -- uso id para devolver el valor que viene

data RoseTree a = Rose a [RoseTree a] -- Tiene un unico constructor recursivo

foldRose :: (a -> [b] -> b) -> RoseTree a -> b 
foldRose f (Rose a h) = f a (map (foldRose f) h) -- con el map foldrose f h me aseguro que se aplica f a los RoseTrees 

alturaRose :: RoseTree a -> Int
alturaRose = foldRose (\_ rs -> if null rs then 1 else 1 + maximum rs)

type Conj a = (a -> Bool)

vacio :: Conj a 
vacio = const False 

agregarConj :: Ord a => a -> Conj a -> Conj a 
agregarConj e c = \x -> if x == e then True else c x 

--agregarConj2 :: Ord a => a -> Conj a -> Conj a


