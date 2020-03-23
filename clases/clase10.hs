--clase10

algoeuclides :: Integer -> Integer -> Integer
algoeuclides n k| mod n k == 0 = k
		|otherwise = algoeuclides k (mod n k)
