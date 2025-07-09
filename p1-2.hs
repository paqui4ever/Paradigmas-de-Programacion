import GHC.Real (infinity)
sum2 :: Num a => [a] -> a 
sum2 = foldr (\acc x -> acc + x) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 e = foldr (\x acc -> if x == e then True else acc) False 

concat2 :: [a] -> [a] -> [a]
concat2 [] xs = xs 
concat2 xs [] = xs
concat2 (y:ys) xs = y : (concat2 ys xs)  

concat2f :: [a] -> [a] -> [a]
concat2f xs ys = foldr (\x acc -> x : acc) ys xs 

-- foldr (\acc x -> x : acc) ys (x:xs) = (\acc x -> x : acc) x (foldr (\acc x -> x : acc) ys xs)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x acc -> if p x then x : acc else acc) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x acc -> (f x) : acc) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a 
mejorSegun p = foldr1 (\x acc -> if p x acc then x else acc) 

sumasParciales2 :: Num a => [a] -> [a]
sumasParciales2 = tail . reverse . foldr (\x acc -> (x + head acc) : acc) [0]   

-- el fold es (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 [] = 0
sumaAlt2 [x] = x
sumaAlt2 (x:y:xs) = (x + y) - sumaAlt2 xs

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x acc -> concatMap (insertarEnTodosLados x) acc) [[]]

insertarEnTodosLados :: a -> [a] -> [[a]]
insertarEnTodosLados e xs = [take i xs ++ [e] ++ drop i xs | i <- [0..length xs]] 

partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x:) acc) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldr (\x acc -> [x] : map (x:) acc) [[]]

prefijosR :: [a] -> [[a]]
prefijosR [x] = [[x]]
prefijosR (x:xs) = map (x:) (prefijosR xs)

sufijos :: Eq a => [a] -> [[a]]
sufijos = foldr (\x (ys:xs) -> (x:ys) : (ys:xs)) [[]]

sufijosR :: Eq a => [a] -> [[a]]
sufijosR [x] = [[x]]
sufijosR xs = xs : (sufijosR (tail xs))

sublistas :: [a] -> [[a]]
sublistas = foldr (\x acc -> acc ++ map (x:) acc ++ [[x]]) [[]]

sublistasR :: Eq a => [a] -> [[a]]
sublistasR [] = [[]]
sublistasR xs = sinRepetidos (prefijos xs ++ sufijos xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = foldr (\x acc -> if notElem x acc then x : acc else acc) [] 

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x ys acc -> if x == e then ys else x : acc) []

contadorApariciones :: Eq a => a -> [a] -> Int
contadorApariciones e = foldr (\x acc -> if x == e then acc + 1 else acc) 0

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs acc -> if e <= x then (e:x:xs) else x : acc) []

insertarOrdenadoR :: Ord a => a -> [a] -> [a]
insertarOrdenadoR x [] = [x]
insertarOrdenadoR x (y:ys) = if x <= y then (x:y:ys) else  y : insertarOrdenadoR x ys

mapPares :: (a -> a -> b) -> [(a,a)] -> [b]
mapPares f = map (\(x,y) -> uncurry2 f (x,y))

curry2 :: ((a,b) -> c) -> (a -> b -> c)
curry2 f x y = f (x,y)

uncurry2 :: (a -> b -> c) -> (a,b) -> c
uncurry2 f (x,y) = f x y

armarPares :: [a] -> [a] -> [(a, a)]
armarPares xs ys = foldr (\x acc ys -> (x, head ys) : acc (tail ys)) (const []) xs ys

-- Se ve asi: armarPares [1,2,3] [4,5,6]
--            f 1 (foldr f [] [2,3]) [4,5,6]
--            (1,4) : [f 2 (foldr [] [3]) [5,6]]
-- ...
-- tiene que aplicarse const [] asi pone todo en una lista y es tail pues es el resto de ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = foldr (\x acc ys -> uncurry2 f (x, head ys) : acc (tail ys)) (const []) xs ys

data Nat a = Zero | Succ (Nat a)

foldNat :: b -> (b -> b) -> Nat a -> b 
foldNat cZero cSucc n = case n of
    Zero -> cZero
    Succ n -> cSucc (foldNat cZero cSucc n)

potencia :: Nat a -> Integer 
potencia = foldNat 1 (\x -> x * 2) 

genLista :: Int -> (Int -> Int) -> Int -> [Int]
genLista inicial f l = take l (iterate f inicial) 

desdeHasta :: (Int, Int) -> [Int]
desdeHasta (x,y) = genLista x (\x -> x + 1) (y-x)

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b 
foldAB cNil cBin arbol = case arbol of
    Nil -> cNil 
    Bin i r d -> cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

recAB :: b -> (b -> AB a -> a -> b -> AB a -> b) -> AB a -> b 
recAB cNil cBin arbol = case arbol of 
    Nil -> cNil 
    Bin i r d -> cBin (recAB cNil cBin i) i r (recAB cNil cBin d) d 

esNil :: AB a -> Bool 
esNil = foldAB True (\_ _ _ -> False) 

altura :: AB a -> Int 
altura = foldAB 1 (\recI r recD -> max recI recD + 1)

cantNodos :: AB a -> Int 
cantNodos = foldAB 0 (\recI r recD -> recI + 1 + recD)

--mejorSegunAB :: (a -> a -> Bool) -> AB a -> a 
--mejorSegunAB p = foldAB Nothing (\recI r recD -> if p r (raizMejor p recI recD) then r else raizMejor p recI recD)


raizMejor :: (a -> a -> Bool) -> a -> a -> a
raizMejor p r1 r2 = if p r1 r2 then r1 else r2  

esABB :: Ord a => AB a -> Bool
esABB = recAB False (\recI i r recD d -> r >= extraerRaiz i && r < extraerRaiz d && recI && recD)

extraerRaiz :: AB a -> a 
extraerRaiz (Bin _ r _) = r

ramas :: AB a ->[[a]]
ramas = foldAB [] (\recI r recD -> map (r:) (recI ++ recD))

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\recI i _ recD d -> (if esHoja i then 1 else recI) + (if esHoja d then 1 else recD))

esHoja :: AB a -> Bool 
esHoja Nil = False
esHoja (Bin Nil _ Nil) = True

mismaEstructura :: AB a -> AB b -> Bool 
mismaEstructura arbol1 (Bin i1 r1 d1) = foldAB esNil fBin arbol1 (Bin i1 r1 d1) 
    where
        fBin recI _ recD (Bin i1 r1 d1) = recI i1 && recD d1
        fBin _ _ _ _ = False

mismoLargo :: [a] -> [b] -> Bool 
-- fold :: (a -> b -> b) -> b -> [a] -> b 
-- fold :: (a -> ([b] -> Bool) -> ([b] -> Bool)) -> ([b] -> Bool) -> [a] -> ([b] -> Bool)
mismoLargo xs ys = foldr (\x acc ys -> acc (tail ys)) esVacia xs ys 

esVacia :: [a] -> Bool 
esVacia [] = True 
esVacia _ = False

mapAB :: (a -> b) -> AB a -> AB b 
mapAB f arbol = foldAB Nil fBin arbol 
    where 
        fBin recI r recD = Bin recI (f r) recD

takeN :: Int -> [a] -> [a]
takeN n xs = foldr (\x acc n -> if n > 0 then x : acc (n-1) else acc (n-1)) (const []) xs n 

--mismaFormaAB :: AB a -> AB a -> Bool 

filterAB :: (a -> Bool) -> AB a -> [a]
filterAB p = foldAB [] (\recI r recD -> (if p r then [r] else []) ++ recI ++ recD)

productoCartesiano :: [a] -> [b] -> [(a, b)] -- es zip
productoCartesiano xs ys= foldr (\x acc ys -> (x, head ys) : acc (tail ys)) (const []) xs ys

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f elem xs = foldr (\x acc elem -> f elem x : acc (f elem x)) (const [elem]) xs elem

-- Recu

data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop

type Valuacion = String -> Bool 

-- a)
foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b 
foldProp cVar cNo cY cO cImp prop = case prop of
    Var s -> cVar s 
    No p -> cNo (acc p)
    Y p q -> cY (acc p) (acc q)
    O p q -> cO (acc p) (acc q)
    Imp p q -> cImp (acc p) (acc q)
    where 
        acc = foldProp cVar cNo cY cO cImp   

recProp :: (String -> b) -> (Prop -> b -> b) -> (Prop -> b -> Prop -> b -> b) -> (Prop -> b -> Prop -> b -> b) -> (Prop -> b -> Prop -> b -> b) -> Prop -> b
recProp cVar cNo cY cO cImp prop = case prop of
    Var s -> cVar s 
    No p -> cNo p (acc p)
    Y p q -> cY p (acc p) q (acc q)
    O p q -> cO p (acc p) q (acc q)
    Imp p q -> cImp p (acc p) q (acc q)
    where 
        acc = recProp cVar cNo cY cO cImp 

-- b)
variables :: Prop -> [String]
variables = foldProp (\x -> [x]) id union union union 
    where 
        union recP recQ = sinRepetidos (recP ++ recQ)
sinRepetidos2 :: Eq a => [a] -> [a]
sinRepetidos2 = foldr (\x acc -> if elem x acc then acc else x : acc) []

-- c) 

evaluar :: Valuacion -> Prop -> Bool 
evaluar val = foldProp val (\recP -> not recP) (\recP recQ -> recP && recQ) (\recP recQ -> recP || recQ) (\recP recQ -> not recP || recQ)

-- d) 
estaEnFNN :: Prop -> Bool 
estaEnFNN = recProp (const True) (\p recP -> esVar p) (\p recP q recQ -> recP && recQ) (\p recP q recQ -> recP && recQ) (\p recP q recQ -> False)
-- No me tengo que olvidar de que las subformulas dentro de los Y y O sean FNN!
esVar :: Prop -> Bool 
esVar (Var _) = True 
esVar _ = False

-- Otro 
data Task = Task String
  deriving (Show, Eq)

data TaskList = EmptyT
              | Add Task TaskList
              | Complete String TaskList
              | Undo Int TaskList
  deriving Show

-- a) 
foldTaskList :: b -> (Task -> b -> b) -> (String -> b -> b) -> (Int -> b -> b) -> TaskList -> b 
foldTaskList cEmpty cAdd cComplete cUndo t = case t of 
    EmptyT -> cEmpty 
    Add task tl -> cAdd task (acc tl)
    Complete string tl -> cComplete string (acc tl)
    Undo num tl -> cUndo num (acc tl)
    where
        acc = foldTaskList cEmpty cAdd cComplete cUndo 

recTaskList :: b -> (Task -> TaskList -> b -> b) -> (String -> TaskList -> b -> b) -> (Int -> TaskList -> b -> b) -> TaskList -> b 
recTaskList cEmpty cAdd cComplete cUndo t = case t of 
    EmptyT -> cEmpty 
    Add task tl -> cAdd task tl (acc tl)
    Complete string tl -> cComplete string tl (acc tl)
    Undo num tl -> cUndo num tl (acc tl)
    where
        acc = recTaskList cEmpty cAdd cComplete cUndo 

-- b) 
tasks :: TaskList -> [String]
tasks = recTaskList [] (\(Task s) _ recTL -> s : recTL) (\str tl recTL -> sacar str recTL) (\n tl recTL -> tasks (deshacerTL n tl))

sacar :: String -> [String] -> [String]
sacar str = foldr (\x acc -> if x == str then acc else x : acc) []

deshacerTL :: Int -> TaskList -> 
deshacerTL n tl = recTaskList (const EmptyT) (\t rest recTL n -> if n <= 0 then Add t rest else recTL (n - 1)) (\s rest recTL n -> if n <= 0 then Complete s rest else recTL (n - 1)) (\m rest recTL n -> if n <= 0 then Undo m rest else recTL (n - 1)) tl n   

--deshacerTL :: Int -> TaskList -> [String]
--deshacerTL n tl = recTaskList (const []) (\(Task s) _ recTL n -> if n > 0 then sacar s recTL else recTL (n-1)) (\str _ recTL n -> if n > 0 then str : recTL else recTL (n-1)) (\num tl recTL n -> if n > 0 then (deshacerTL num tl) ++ recTL (n-1)) tl n 

count :: TaskList -> Int 
count tl = length tasks tl 

valid :: TaskList -> Bool 
valid = recTaskList True (\task _ recTL -> recTL) (\str tl recTL -> elem str (tasks tl)) (\n tl recTL -> n < size tl)

sizeTL :: TaskList -> Integer 
sizeTL = foldTaskList 0 (\task recTL -> 1 + recTL) (\str recTL -> recTL + 1) (\n recTL -> recTL + 1)

undoAll :: TaskList -> TaskList
undoAll _ = EmptyT