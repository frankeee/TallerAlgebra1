--ej1

espar :: Integer -> Bool
espar n|mod n 2 == 0 = True
       |otherwise = False

sumatoriapares :: [Integer] -> Integer
sumatoriapares n|length n == 0 = 0
		|espar (head n) = head n + sumatoriapares (tail n)
		|otherwise = sumatoriapares (tail n)

--ej2


filtro :: Integer -> [Integer] -> [Integer]
filtro n k|length k == 0 = []
	  |n == head k = filtro n (tail k)
	  |otherwise = head k : filtro n (tail k)

--ej3



muestraindice :: Integer -> Integer -> [Integer] -> [Integer]
muestraindice i n k|length k == 0 = []
		   |head k == n = i : muestraindice (i+1) n (tail k)
		   |head k /= n = muestraindice (i+1) n (tail k)


posiciones :: Integer -> [Integer] -> [Integer]
posiciones n k = muestraindice 1 n k
	      


