-Actividad 1


data Nat = Zero|Succ Nat
instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "Succ"++"("++show n++")"
  --show = show.natToInt

--Actividad 2

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

--Actividad 3

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))


--Actividad 4

sumaNat :: Nat -> Nat -> Nat
sumaNat n1 n2 = intToNat (natToInt n1 + natToInt n2)

--Actividad 5

data Arbol a = Nil|Node (Arbol a) a (Arbol a)
{-instance Show Arbol where
    show Nil = "*"
    show Node a = show a1 ++ "<--" ++ a ++ "-->"++ show a2
-}

--Actividad 6

size :: Arbol a -> Int
size Nil = 0
size (Node n1 n n2) = 1 + size n1 + size n2

--height ::