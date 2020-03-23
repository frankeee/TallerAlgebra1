
--ej1
fibo :: Integer-> Integer
fibo n|n == 0 = 1
      |n == 1 = 1
      |otherwise = fibo (n-1) + fibo (n-2)

fiboaux :: Integer -> Integer -> Bool
fiboaux n s|n == fibo s = True
	   |n > fibo s = fiboaux n (s+1)

           |n < fibo s = False

esfib n|fiboaux n 1 = True
       |otherwise = False


--ej2

mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla n k|length n == 0 && length k == 0 = []
	  |length n == 0 = k
	  |length k == 0 = n
	  |otherwise = head n : head k : mezcla (tail n) (tail k)

--ej4

minimo :: [Integer] -> Integer
minimo n|length n == 1 = head n
	|head n > minimo (tail n) = head n
	|otherwise = minimo (tail n)

--incmin :: [Integer] -> [Integer]
--incmin n|length == 0 = []
--	|otherwise = (head n) + minimo n



