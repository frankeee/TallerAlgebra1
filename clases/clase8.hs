--clase8

definirvacio :: [Integer]
definirvacio = []

incluido :: [Integer] -> [Integer] -> Bool
incluido n k |length n == 0 = True
	     |elem (head n) k = incluido (tail n) k
	     |otherwise = False


iguales :: [Integer] -> [Integer] -> Bool
iguales n k  |length n /= length k = False
	     |otherwise = incluido n k

agregaratodas :: [Integer] -> [[Integer]] -> [[Integer]]
agregaratodas n k|length k == 0 = []
		 |otherwise = (head k) ++ n : agregaratodas n (tail k)



partes :: Integer -> Set (Set Integer)
partes n | n == 0 = [[]]
         | otherwise = partes (n-1) ++ agregarATodas n (partes (n-1))

--Variaciones

aATLista :: Set Integer -> Set [Integer] -> Set [Integer]

aATLista [] xs = []
aATLista (l:ls) xs = agregarATodas l xs ++ aATLista ls xs


variaciones :: Set Integer -> Integer -> Set [Integer]

variaciones cs 0 = [[]]
variaciones cs l = aATLista cs (variaciones cs (l - 1))  

