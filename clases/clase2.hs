f n | n == 0 = 1



f1 :: int -> (int , int , int)
f1 n |(2*n ,n ^ 2, n-7)

f2 :: int -> int
f2 n |mod n 2 == 0 = n/2
     |mod n 2 == 1 = n + 1
