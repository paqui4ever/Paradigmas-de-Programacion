rev :: [a] -> [a]
rev = foldr (\x acc -> acc ++ [x]) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x:) acc) []