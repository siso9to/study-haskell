fib1::Int->Int
fib1 0 = 0
fib1 1 = 1
fib1 x = fib1(x - 2) + fib1(x - 1)

fib2::Int->Int
fib2 x
  | x == 0 = 0 
  | x == 1 = 1
  | otherwise = fib2(x - 2) + fib2(x - 1)

fib3::Int->Int
fib3 x = case x of 0 -> 0
                   1 -> 1
                   x -> fib3(x - 2) + fib3(x - 1)

fib4::Int->Int
fib4 x = fibx x
  where fibx 0 = 0
        fibx 1 = 1
        fibx x = fib4(x - 2) + fib4(x - 1)
