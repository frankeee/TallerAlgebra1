division  ::  Integer  -> Integer  -> (Integer , Integer)
division a d | a < d = (0, a)
	     | otherwise = (fst (division (a-d) d) + 1,snd (division (a-d) d))

