meuLast [] = error "Lista vazia!"
meuLast [x] = x
meuLast (h:xs) = meuLast xs

penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo [x,y] = x
penultimo xs = penultimo $ tail $ xs

elementAt 1 xs = head xs
elementAt i xs = elementAt (i-1) (tail xs)

meuLength [] = 0
meuLength xs = 1 + meuLength (tail xs)

meuReverso [] = []
meuReverso xs = meuReverso (tail xs) ++ [head xs]

isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head xs == last xs && isPalindrome (tail $ init $ xs)

compress [] = []
compress (x:xs) = [x] ++ compress (filter (/=x) xs)

compact [] = []
compact (x:xs) = [x] ++ (filter (==x) xs) ++ compact (filter (/=x) xs)

encode [] = []
encode (x:xs) = [(x,1+meuLength(filter (==x) xs))] ++ encode (filter (/=x) xs)

--ver como fazer recursivamente
split xs i = [take i xs] ++ [drop i xs]

slice xs imin imax = take (imax-imin+1) (drop (imin-1) xs)

insertAt el pos xs = take (pos-1) xs ++ [el] ++ drop (pos-1) xs

minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs)

mySum xs = foldr (+) 0 xs

maxList [] = error "Lista vazia"
maxList (x:xs) = foldr (max) x xs

buildPalindrome xs = xs ++ meuReverso xs

mean :: [Int] -> Int
mean xs = (mySum xs) `div` meuLength xs

myAppend xs ys = foldr (:) ys xs 

main = do
  print(myAppend [4,2,3] [5,6,7])