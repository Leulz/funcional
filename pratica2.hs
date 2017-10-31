divTuple :: (Int, Int) -> Maybe Int
divTuple (_,0) = Nothing
divTuple (x,y) = Just (x `div` y)

somatorio :: Int -> Int -> Int
somatorio a b = sum([a..b])

somatorioRec :: Int -> Int -> Int
somatorioRec a b = somatorioRecAux a b 0

somatorioRecAux :: Int -> Int -> Int -> Int
somatorioRecAux a b acum 
  | a == b = acum + a
  | a > b = acum
  | otherwise = somatorioRecAux (a+1) (b) (acum+a)

square :: Int -> Int
square x = x ^ 2

sumSquares :: Int -> Int -> Int
sumSquares x y = (square x) + (square y)

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = f a + f b

hoSumSquares :: Int -> Int -> Int
hoSumSquares a b = higherOrderSum square a b

mapFilter :: (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
mapFilter f p xs = filter p (map f xs)

main = do
  print(mapFilter square (<5) [1,2,3,4])