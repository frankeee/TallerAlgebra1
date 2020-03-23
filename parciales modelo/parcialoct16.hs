--ej1

kesimo :: Integer -> [Integer] -> Integer
kesimo n k|length k == 0 = -66
	  |n == 1 = head k
	  |otherwise = kesimo (n-1) (tail k)

--ej2

domina :: [Integer] -> [Integer] -> Bool
domina n k|length k == 0 = True
	  |head n > head k = domina (tail n) (tail k)
	  |otherwise = False

--ej3
