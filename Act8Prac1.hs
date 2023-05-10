digito :: Int -> [Int]

digito num = if num < 10
    then [num]
    else [mod num 10] ++ digito (div num 10)


digito1 :: Int -> [Int]

digito1 num = reverse (digito (num))