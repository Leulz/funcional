import Data.List as List

data Bag a = Bag [(a,Int)] deriving (Eq,Show)

insert elem (Bag xs) = 

main = do
	let x=Bag[("a",1),("b",2)] in putStr "test"