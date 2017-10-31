import Data.Function (fix)

square = \x -> x*x

pow = \x y -> x ^ y

fatorial = fix (\t n -> if n == 1 then 1 else n * t(n-1))

main = do
  print(fatorial 5)