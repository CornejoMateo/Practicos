-- Actividad 1

merge1 :: [Int] -> [Int] -> [Int]
merge1 x y
    |null x = y
    |null y = x
    |otherwise = if head x <= head y then head x : merge1 (tail x) y
        else head y : merge1 x (tail y)

-- Actividad 2

insertEnd :: [a] -> a -> [a]
insertEnd [] n = [n]
insertEnd xs n = reverse (n : reverse(xs))

bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x >= y then y : bubble (x:xs) else x : bubble (y:xs) 

--orden :: [Int] -> [Int]
--orden [] = []
--orden [x] = [x]
--orden (x:xs) = insertEnd (last1 (bubble (x:xs))) (orden (init2 (bubble (x:xs))))

hd1 :: [a] -> a
hd1 (x:xs) = x

tl1 :: [a] -> [a]
tl1 [] = []
tl1 (x:xs) = xs 

last1 :: [a] -> a
last1 tex = hd1(reverse (tex))  

init2 :: [a] -> [a]
init2 [] = []
init2 xs = reverse (tl1(reverse (xs)))

-- Actividad 3

poten2 :: Int -> Int
poten2 num = if num == 0 then 1 else 2 * poten2 (num - 1)

-- Actividad 4

bin :: Int -> [Int]
bin num = if num == 0 then [] else (mod num 2) : bin (div num 2)

binario :: Int -> [Int]
binario xs = reverse (bin xs)

-- Actividad 5

par :: [Int] -> Bool
par [0] = True
par [1] = False
par (x:y:xs) = if last (xs) == 0 then True else False

verBin :: Int -> Bool
verBin num = par (binario num)

-- Actividad 6

distH :: Eq a => [a] -> [a] -> Int
distH [] ys = 0
distH xs [] = 0
distH xs ys = if head xs == head ys then 0 + distH (tail xs) (tail ys) else 1 + distH (tail xs) (tail ys) 

-- Actividad 7

cuadPer :: Int -> Bool
cuadPer num = length [x | x <- [1..num], x^2 == num] == 1

-- Actividad 8

rep :: Eq a => [a] -> a -> Int -> Bool
rep xs z num |num == 0 && xs == [] = True |num /= 0 && xs == [] = False |otherwise = if head (xs) == z then rep (tail xs) z (num - 1) else rep (tail xs) z (num)

-- Actividad 9

nelem :: [Int] -> Int -> Int
nelem [] n = -1 --Si retorna -1 no existe la posición dada en la lista
nelem xs n = if n == 0 then head xs else nelem (tail xs) (n - 1) 

--nelem :: [Int] -> Int -> Int
--nelem xs n = if n == (last xs) then 0 else 1 + nelem (init2 xs) n


-- Actividad 10 

posicionesC :: [Char] -> Char -> [Int]
posicionesC [] c = []
posicionesC xs c = posicionesImp xs c 0

posicionesImp :: [Char] -> Char -> Int -> [Int]
posicionesImp [] c cant = []
posicionesImp xs c cant = if head xs == c then cant : posicionesImp (tail xs) c (cant + 1) else posicionesImp (tail xs) c (cant + 1)

-- Actividad 11

compact :: Eq a => [a] -> [a]
compact [] = [] 
compact [x] = [x]
compact (x:y:xs) = if x == y then compact (y:xs) else x : compact (y:xs) 