--Act 2)-----------------------------

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

--Act 3)---------------------------

maxTres1 :: Int -> Int -> Int -> Int

maxTres1 x y z = if x >= y && x >= z then x
                 else 
                    if y >= x && y >= z then y
                    else z


--Act 4)-----------------------------

concatenar1 :: [a] -> [a] -> [a]
concatenar1 [] ys = ys
concatenar1 xs [] = xs
concatenar1 (x:xs) ys = x : concatenar1 xs ys 


tomar1 :: Int -> [a] -> [a]
tomar1 n (x:s)   | n == 0 = []
                 | null (x:s) = []
                 | otherwise = x : tomar1 (n - 1) s 


tirar1 :: Int -> [a] -> [a]
tirar1 num (x:s) | num == 0 = []
             | null (x:s) = []
             | otherwise = x : tirar1 (num - 1) s


--Act 5)-----------------------------

abs1 :: Int -> Int
abs1 num = if num >= 0 then num
           else (-num)


--Act 6)-----------------------------




--Act 7)-----------------------------

xor ::  Bool -> Bool -> Bool 
xor x r = if x == True && r == False then True
      else 
            if x == False && r == True then True
            else
                if x == True && r == True then False
                else False


xor2 ::  Bool -> Bool -> Bool 
xor2 x r = if x == True && r == False then True
      else 
            if x == False && r == True then True
            else False


--Act 8)-----------------------------

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


--Act 9)-----------------------------

primosMen :: Int -> [Int]
primosMen num = if num <= 1 then [num]
                else if primo num == True then num : primosMen (num - 1)
                    else primosMen (num - 1)

primosMenores :: Int -> [Int]
primosMenores n = revs (primosMen n) 



--Act 10)----------------------------

revs :: Eq a => [a] -> [a]
revs xs = if xs == [] then []
              else last xs : revs(init2 xs) 

--Act 11)----------------------------

iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys
    | xs == [] && ys == [] = True
    | length xs /= length ys || hd1 (xs) /= hd1 (ys) = False
    | otherwise = iguales (tl1 xs) (tl1 ys)


--Act 12)----------------------------

palind :: Eq a => [a] -> Bool
palind xs = if iguales xs (revs xs) then True
            else False

--Act 13)

raiz :: Float -> Float -> Float -> String
raiz a b c = if (b^2 - 4*a*c) < 0 then "No tiene raiz"
             else if (b^2 - 4*a*c) == 0 then "Tiene una raiz"
             else "Tiene dos raices"