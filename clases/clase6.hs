--Clase 6

ylogico :: Bool -> Bool -> Bool
ylogico True False = False
ylogico _ _ = True

ologico :: Bool -> Bool -> Bool 
ologico False False = False
ologico _ _ = True

sumagaussiana :: Integer -> Integer 
sumagaussiana 0 = 0
sumagaussiana n = n + sumagaussiana(n-1)

algunoescero :: (Integer , Integer , Integer ) -> Bool
algunoescero (0,_,_) = True
algunoescero (_,0,_) = True
algunoescero (_,_,0) = True
algunoescero (_,_,_) = False

productointerno :: (Float ,Float) -> (Float,Float) -> Float
productointerno (x1 , x2) (y1 , y2) = x1 * y1 + x2 * y2

menordivisor :: Integer -> Integer -> Integer
menordivisor n k| mod n k == 0 = k
		| otherwise = menordivisor n (k+1)

esPrimo :: Integer -> Bool
esPrimo n |n == 1 = False
	  |menordivisor n 2 == n = True
	  |otherwise = False

--essumadedosprimos :: Integer -> Bool


sumadigitos :: Integer -> Integer
sumadigitos n | mod n 10 == n = n
	      | otherwise = mod n 10 + sumadigitos (div n 10)	

digitosiguales :: Integer -> Bool
digitosiguales n |div n 10 == 0 = True
		 |mod n 10 == mod (div n 10) 10 = digitosiguales(div n 10) 
 		 |otherwise = False
