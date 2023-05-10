-- Actividad 1

inf1 :: [Int]
inf1 = 1 : inf1


-- Actividad 2

infNat :: Int -> [Int]
infNat num = num : infNat (num + 1)

-- Actividad 3

nat2 :: Int -> [Int]
nat2 num = reverse (nat num)
           where nat :: Int -> [Int]
                 nat 0 = []
                 nat num = num : nat (num - 1)

-- Actividad 4

infEnt :: [Int] -> [Int]
infEnt xs = infEnt2 xs 5
            where infEnt2 :: [Int] -> Int -> [Int]
                  infEnt2 xs 0 = []
             	  infEnt2 xs n = head (xs) : infEnt2 (tail xs) (n - 1)

-- Actividad 5

cuadEnt :: [Int] -> [Int]
cuadEnt [] = []
cuadEnt (x:xs) = cuad x : map (cuad) xs

cuad :: Int -> Int 
cuad x = x*x

-- Actividad 6

esDiv :: Int -> Int -> Bool
esDiv n1 0 = error "no se puede dividir por 0"
esDiv 0 n2 = True
esDiv n1 n2 = if mod n1 n2 == 0 then True else False

divisores :: Int -> [Int]
divisores num = filter (esDiv num) ([1..num]) 

-- Actividad 7

primos2 :: [Int] -> [Int]
primos2 [] = []
primos2 xs = filter primo xs
			   
primo :: Int -> Bool
primo num = if mod num 2 == 0 && num /= 2 then False
            else 
                if mod num 3 == 0 && num /= 3 then False
            	else
                    if mod num 5 == 0 && num /= 5 then False
                	else   
						if mod num 7 == 0 && num /= 7 then False
						else
						    if mod num 3 == 0 || mod num 5 == 0 || mod num 7 == 0 then True
						        else True

-- Actividad 8

sumC :: [Int] -> Int
sumC [] = 0
sumC xs = foldr (+) 0 (cuadEnt xs)

-- Actividad 9

sucNat :: [Int] -> [Int]
sucNat [] = []
sucNat (x:xs) = sucesor x : (map sucesor xs)

sucesor :: Int -> Int
sucesor x = x + 1

-- Actividad 10

sumLisEnt :: [Int] -> Int
sumLisEnt [] = 0
sumLisEnt xs = foldr (+) 0 (xs)

-- Actividad 11 

fact :: Int -> Int
fact 0 = 1
fact n = foldr (*) 1 [1..n]

-- Actividad 12

and1 :: [Bool] -> Bool
and1 xs = foldr (&&) True xs

-- Actividad 13

contar :: Int -> a -> Int
contar ac _ = ac + 1

tamm :: [a] -> Int
tamm [] = 0
tamm xs = foldl (contar) 0 xs

-- Actividad 14

suces :: [Int] -> [Int]
suces xs = [x + 1 | x <- xs]

-- Actividad 15

cuadNat :: [Int] -> [Int]
cuadNat xs = [x^2 | x <- xs]

-- Actividad 16 

paresMay :: [Int] -> [Int]
paresMay xs = [x | x <- xs, mod x 2 == 0 && x > 10]

-- Actividad 17

divis :: Int -> [Int]
divis num = [x | x <- [1..num], mod num x == 0]

-- Actividad 18

todosOcurreEn :: Eq a => [a] -> [a] -> Bool
todosOcurreEn [] ys = True
todosOcurreEn xs ys = if [x | x <- xs, elem x ys] /= xs then False else True


-- Actividad 19

primos  :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]
           where primo :: Int -> Bool
                 primo num = if mod num 2 == 0 && num /= 2 then False
                            else 
                                if mod num 3 == 0 && num /= 3 then False
                                else
                                    if mod num 5 == 0 && num /= 5 then False
                                    else   
                                        if mod num 7 == 0 && num /= 7 then False
                                        else
                                            if mod num 3 == 0 || mod num 5 == 0 || mod num 7 == 0 then True
                                            else True


-- Actividad 20

prodCart :: [Int] -> [Int] -> [(Int,Int)]
prodCart xs ys = [(x,y) | x <- xs, y <- ys]

-- Actividad 21 

ocurr :: Eq a => [a] -> a -> Int
ocurr [] x = 0
ocurr (y:ys) d = length [x | x <- ys, x == d]

-- Actividad 22

split2 :: Eq a => [a] -> [([a],[a])]
split2 [] = []
split2 xs = [(take i xs, drop i xs) | i <- [0..length xs]]

-- split2 xs = [ ([x],[y]) | x <- god xs, y <- god xs, [x] ++ [y] == xs]
-- god :: [a] -> [[a]]  
-- god xs = [] : parSplit xs
--              where parSplit :: [a] -> [[a]]
--                    parSplit [] = []
--                    parSplit xs = xs : parSplit (tail xs)

-- Actividad 23

sumSeg :: [Int] -> Int
sumSeg [] = 0
--sumSeg ys = sum [x * (length ys - (head(posicionesC ys x)))| x <- ys]
sumSeg ys = sum [sum (take i ys) | i <- [1..length ys]]

-- Actividad 24

infPares :: [Int]
infPares = [x | x <- [0..], mod x 2 == 0]








posicionesC :: [Int] -> Int -> [Int]
posicionesC [] c = []
posicionesC xs c = posicionesImp xs c 0

posicionesImp :: [Int] -> Int -> Int -> [Int]
posicionesImp [] c cant = []
posicionesImp xs c cant = if head xs == c then cant : posicionesImp (tail xs) c (cant + 1) else posicionesImp (tail xs) c (cant + 1)