
--practica

--parcial simulacro
--ej1

agarraprimero :: (Float,Float,Float) -> Float
agarraprimero (a,_,_) = a


agarrasegundo :: (Float,Float,Float) -> Float
agarrasegundo (_,a,_) = a


agarratercero :: (Float,Float,Float) -> Float
agarratercero (_,_,a) = a

menorlex :: (Float,Float,Float) -> (Float,Float,Float) -> Bool
menorlex n k|agarraprimero n < agarraprimero k = True
	    |agarraprimero n == agarraprimero k && agarrasegundo n < agarrasegundo k = True
	    |agarraprimero n == agarraprimero k && agarrasegundo n == agarrasegundo k && agarratercero n < agarratercero k = True
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
esdiv n k | mod n k == 0 = True
          | otherwise = False


sumadiv :: Integer -> Integer -> Integer
sumadiv n k|k  == 1 = 1	
           |esdiv n k = k + sumadiv n (k-1)
           |esdiv n k == False = sumadiv n (k-1)	

esdefectivo :: Integer -> Bool
esdefectivo n |n >= sumadiv n (n-1) = True
              |otherwise = False

--ej4

modif :: Integer -> Integer ->Integer
modif n k |n > k = n - k
	  |n < k = k - n
	  |n == k = 0

listanueva :: [Integer] -> [Integer]
listanueva n |length n == 1 = []
	     |otherwise = modif (head n) (head (tail n)) : listanueva (tail n)

maxlista :: [Integer] -> Integer
maxlista n|length n == 1 = head n
	  |head n > maxlista (tail n) = head n
	  |otherwise = maxlista (tail n)

maximadistancia :: [Integer] -> Integer
maximadistancia n = maxlista (listanueva n)
--ej 5

sonamiges :: Integer -> Integer -> Bool
sonamiges n k|sumadiv n (n-1) == k && sumadiv k (k-1) == n = True
	     |otherwise = False


