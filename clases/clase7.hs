--clase7

listar :: a -> a -> a -> [a] 
listar n x c = [n,x,c]

sumatoria :: [Integer] -> Integer
sumatoria n  | length n == 0 = 0
	     | otherwise = head n + sumatoria (tail n)


--pertenece :: [a] -> Bool
--pertenece n k|length k == 0 = False
  --           |n == head k = True
	--     |otherwise = pertenece (tail k)	

productoria :: [Integer] -> Integer
productoria n| length n == 0 = 1
	     | otherwise = head n * sumatoria (tail n)



buscaultimo :: [Integer] -> Integer
buscaultimo n|length n == 1 = head n
	     |otherwise = buscaultimo (tail n)

suman :: Integer -> [Integer] -> [Integer]
suman n k |length k == 1 = (n + head k) : []
	  |otherwise = n + head k : suman n (tail k)


sumaultimo :: [Integer] -> [Integer]
sumaultimo k = suman (buscaultimo k) k

buscaelprimero :: [Integer] -> Integer
buscaelprimero k = head k

sumaelprimero :: [Integer] -> [Integer]
sumaelprimero k = suman (buscaelprimero k) k

espar :: Integer -> Bool
espar n|mod n 2 == 0 = True
       |mod n 2 == 1 = False

pares :: [Integer] -> [Integer]
pares k |length k == 0 = []
	|espar (head k) = head k : pares (tail k)
	|espar (head k) == False = pares (tail k)

esmulti :: Integer -> Integer -> Bool
esmulti n k|mod n k == 0 = True
	      |mod n k /= 0 = False 

multiplosden :: Integer -> [Integer] -> [Integer]
multiplosden n k |length k == 0 = []
	     	 |esmulti (head k) n = head k : multiplosden n (tail k)
		 |esmulti (head k) n == False = multiplosden n (tail k)

quitar :: Integer -> [Integer] -> [Integer]
quitar n k|length k == 0 = [] 
	  |head k /= n = head k : quitar n (tail k)
	  

