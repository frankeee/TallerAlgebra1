--simulacroparcial

--ej1

primero :: (Float,Float,Float) -> Float
primero (a,_,_) = a

segundo :: (Float,Float,Float) -> Float
segundo (_,a,_) = a

tercero :: (Float,Float,Float) -> Float
tercero (_,_,a) = a

menorlex :: (Float,Float,Float) ->  (Float,Float,Float) -> Bool
menorlex n k|primero n < primero k = True
	    |primero n == primero k && segundo n < segundo k = True
	    |primero n == primero k && segundo n == segundo k && tercero n < tercero k = True
	    |otherwise = False


--ej2


fib :: Integer -> Integer
fib n|n == 0 = 1
     |n == 1 = 1
     |otherwise = fib (n-1) + fib (n-2)


sumafibonacci :: Integer -> Integer
sumafibonacci n|n == 0 = fib 0
	       |n == 1 = fib 1
	       |otherwise = fib n + sumafibonacci (n-1) 

--ej3


esdiv :: Integer -> Integer -> Bool
esdiv n k|mod n k == 0 = True
	 |otherwise = False


sumadiv :: Integer -> Integer -> Integer
sumadiv n k|n == k = 0
	   |esdiv n k = k + sumadiv n (k+1)
	   |esdiv n k == False = sumadiv n (k+1)

esdefectivo :: Integer -> Bool
esdefectivo n|n > sumadiv n 1 = True
	     |otherwise = False


--ej4 


nuevalista :: [Integer] -> [Integer]
nuevalista n|length n == 1 = []
	    |otherwise = head n - head (tail n) : nuevalista (tail n)


sacamaximo :: [Integer] -> Integer
sacamaximo n|length n == 1 = head n
	    |head n > sacamaximo (tail n) = head n
	    |otherwise = sacamaximo (tail n)

maximadistancia :: [Integer] -> Integer
maximadistancia n = sacamaximo (nuevalista n)


--ej5


--sonamigos :: Integer -> Integer -> Bool
--sonamigos n k|sumadiv n == k && sumadiv k == n = True
--	     |otherwise = False



--"ej 6"
 


menordiv n k|n == k = n
	    |esdiv n k = k
	    |otherwise = menordiv n (k+1)


esprimo n|n == 1 = False
	 |menordiv n 2 == n = True
	 |otherwise = False

armalista n k|k == 1 = []
	     |esdiv n k && esprimo k = k : armalista n (k-1)
	     |otherwise = armalista n (k-1)
	     

comparalista n k|length k == 0 = True
	        |head k > n = False
	        |n > head k = comparalista n (tail k)


--esbsuave :: Integer -> Integer -> Bool
--esbsuave n k|comparalista k (armalista n n-1) = True
	    |otherwise = False


