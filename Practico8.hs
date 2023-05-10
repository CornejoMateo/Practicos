-- Act 1

nand :: Bool -> Bool -> Bool
nand False _ = True
nand _ False = True
nand _ _ = False

-- Act 2

maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _ = False

-- Act 3

-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------

--paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Bool
paraTodo ys xs p = and [ p i xs | i <- ys]

--esPar :: Int -> [Int] -> Bool
--esPar i [] = error "Posicion inexistente"
--esPar 0 (x:xs) = mod x 2 == 0
--esPar i (x:xs) = esPar (i - 1) xs 

esPar1 :: Int -> [Int] -> Bool
esPar1 i xs = mod (xs !!i) 2 == 0

-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
--existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool

existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe ys xs p = or [ p i xs | i <- ys]

-- Act 4

contatoria :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Int
contatoria ys xs p = length [ 1 | i <- ys, p i xs]

sumatoria :: Num a => [Int] -> [a] -> (Int -> [a] -> Bool) -> a
sumatoria ys xs p = sum [ (xs !!i) | i <- ys, p i xs]

productoria :: Num a => [Int] -> [a] -> (Int -> [a] -> a) -> a
productoria ys xs p = product[ p i xs | i <- ys]

cuad :: Num a => Int -> [a] -> a
cuad i xs = (xs !!i) * (xs !!i)
