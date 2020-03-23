
--practica

--parcial octubre 2017


--ej1

primero :: (Float,Float,Float) -> Float
primero (a,_,_) = a

segundo :: (Float,Float,Float) -> Float
segundo (_,a,_) = a

tercero :: (Float,Float,Float) -> Float
tercero (_,_,a) = a

todomenor :: (Float,Float,Float) -> (Float,Float,Float) -> Bool
todomenor n k|primero n < primero k && segundo n < segundo k && tercero n < tercero k = True
	     |otherwise = False

--ej2 

fibonacci :: Integer -> Integer
fibonacci n |n == 0 = 1
	    |n == 1 = 1
	    |otherwise = fibonacci(n-1) + fibonacci(n-2)

sumafibonacci :: Integer -> Integer
sumafibonacci n|n == 0 = fibonacci 0
	       |n == 1 = fibonacci 1
	       |otherwise = fibonacci n + sumafibonacci (n-1)
--ej3


esdiv :: Integer -> Integer -> Bool
esdiv n k| mod n k == 0 = True
	 |otherwise = False

menordiv :: Integer -> Integer -> Integer
menordiv n k|n == k = n
	    |esdiv n k = k
	    |otherwise = menordiv n (k+1)

esprimo :: Integer -> Bool
esprimo n|n == 1 = False
	 |menordiv n 2 == n = True
	 |otherwise = False

armalista :: Integer -> Integer -> [Integer]
armalista n k| n == k = []
	     |esdiv n k && esprimo k = k : armalista n (k+1)		
	     |otherwise = armalista n (k+1) 
	
comparalista ::  Integer -> [Integer] -> Bool
comparalista n k|length k == 0 = True
	        |n > head k = comparalista n (tail k)
	        |otherwise = False

esbsuave :: Integer -> Integer -> Bool
esbsuave n k|comparalista  k (armalista n 1) == True = True
	    |otherwise = False  




