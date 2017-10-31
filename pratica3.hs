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
split xs = [take i xs] ++ [drop i xs]



main = do
  print(split [1,2,1,8,34,12,8])