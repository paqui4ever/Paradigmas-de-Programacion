rev :: [a] -> [a]
rev = foldr (\x acc -> acc ++ [x]) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x:) acc) []

lagrange :: [(Float, Float)] -> Float -> Float
lagrange x xs = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs 
    where l xj = foldl (\acc (xk, _) -> if xk == xj then acc else acc * ((x - xk) / (xj - xk))) 1 xs