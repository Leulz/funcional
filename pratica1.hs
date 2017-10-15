import Data.List

xor :: Bool -> Bool -> Bool
xor a b = (b && not a) || (a && not b)

impl :: Bool -> Bool -> Bool
impl a b = (not a) || b

equiv :: Bool -> Bool -> Bool
equiv a b = (impl a b) && (impl b a)

pow :: Int -> Int -> Int
pow a b = pow_aux 1 a b

pow_aux :: Int -> Int -> Int -> Int
pow_aux y x 0 = y
pow_aux y x 1 = x*y
pow_aux y x n = 
  if n `mod` 2 == 0 then pow_aux (y) (x*x) (n `div` 2)
    else pow_aux (x*y) (x*x) ((n-1) `div` 2)

fatorial :: Int -> Int
fatorial 1 = 1
fatorial x = x * fatorial x-1

isPrime :: Int -> Bool
isPrime x = null [ k | k <- [2..x - 1], x `mod`k  == 0]

erathostenes :: Int -> [Int]
erathostenes m = sieve [2..m]
             where 
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = (fib (x-1)) + (fib (x-2))

mdc :: Int -> Int -> Int
mdc x 0 = x
mdc x y = mdc (y)  (x `mod` y)

mmc :: Int -> Int -> Int
mmc x y = (x `div` (mdc x y)) * y

coprimo :: Int -> Int -> Bool
coprimo x y = mdc x y == 1

subsetSum :: Int -> [Int] -> [Int]
subsetSum x [] = []
subsetSum x (head:tail) = if (head + head == x) then [head,head] 
  else if not $ null $ (filter (\k -> head + k == x) tail) then [head] ++ (filter (\k -> head + k == x) tail) 
  else subsetSum x tail

goldbach :: Int -> [Int]
goldbach x = if x <= 2 || (x `mod` 2) /= 0 then [] else
  subsetSum (x) (erathostenes x)


main = do
  a <- getLine
  print(goldbach (read a))