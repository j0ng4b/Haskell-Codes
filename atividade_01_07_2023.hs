{-
    1)
    Faça uma função que receba uma lista de
    produtos e um produto e retorne verdadeiro
    caso esse elemento pertença a lista.
-}
contain :: [String] -> String -> Bool
contain [] _ = False
contain (v:vals) prod
    | prod == v = True
    | otherwise = contain vals prod

{-
    2)
    Faça uma função que receba um inteiro n
    qualquer e uma lista de inteiros e retorne outra
    lista de inteiros onde cada número está
    multiplicado pelo inteiro passado.
-}
multiply :: Int -> [Int] -> [Int]
multiply _ [] = []
multiply n (v:vals) = (n * v) : (multiply n vals)

{-
    3)
    Faça uma função que leia uma lista de inteiros
    e retorne o maior elemento da lista.
-}
maximumNum :: [Int] -> Int
maximumNum [x] = x
maximumNum (num:nums) =
    -- Outro maneira de fazer envolve chamar a função
    -- recursivamente duas vezes mas isso aumenta o
    -- processamento além de provavelmente aumentar o
    -- o consumo de mémoria.
    let x = maximumNum nums
    in if num > x then num else x

-- 3) Implementação alternativa using where clause
maximumNum' :: [Int] -> Int
maximumNum' [x] = x
maximumNum' (num:nums)
    | num > last = num
    | otherwise = last
    where last = maximumNum' nums

{-
    4)
    Faça uma função que receba uma lista de
    inteiros e exiba a lista invertida.
    Ex: [1,2,3,4] sairá [4,3,2,1]
-}
reverseList :: [Int] -> [Int]
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

{-
    5)
    Construa uma função que dado três valores
    verifique se eles podem formar um triângulo, caso seja
    verdadeiro retorne uma tupla-2 com o tipo do
    triângulo formado e o perímetro do mesmo.
-}
triangleType :: Int -> Int -> Int -> String
triangleType a b c
    | l1 && l2 && l3 = "Triangulo equilatero"
    | l1 || l2 || l3 = "Triangulo isosceles"
    | otherwise = "Triangulo escaleno"
    where
        l1 = a == b
        l2 = a == c
        l3 = b == c

isTriangle :: Int -> Int -> Int -> (String, Int)
isTriangle a b c
    | a < b + c && b < a + c && c < a + b = (triangleType a b c, a + b + c)
    | otherwise = ("Nao e um triangulo", 0)

{-
    6)
    Crie uma função que leia uma lista contendo nomes
    de produtos e seus preços e exiba o valor total da
    compra.
-}
type Product = (String, Float)

cartTotal :: [Product] -> Float
cartTotal [prod] = snd prod
cartTotal (prod:prods) = snd prod + cartTotal prods

{-
    7)
    Adapte a função anterior para ler também a
    quantidade de cada produto comprado e exibir o valor
    total da compra.
-}

type Product' = (String, Float, Int)

cartTotal' :: [Product'] -> Float
cartTotal' [(_, val, count)] = val * fromIntegral count
cartTotal' ((_, val, count):prods) = val * fromIntegral count + cartTotal' prods

{-
    8)
    Faça uma função que leia o nome do produto, valor
    unitário e quantidade comprada de uma lista de
    produtos e exiba:
-}
cartFinance :: [Product'] -> [Product]
cartFinance [] = []
cartFinance ((name, val, count):prods) = (name, val * fromIntegral count) : cartFinance prods

main :: IO ()
main = do
    putStrLn "1) Tem pão?"
    print(contain ["Brigadeiro", "Biscoito", "Chocolate", "Maça", "Pão"] "Pão")
    putStrLn "\n2) Elementos da lista multiplicados:"
    putStr "\tLista: "
    print([1..5])
    putStr "\tResultado (x5): "
    print(multiply 5 [1..5])
    putStr "\tResultado (x10): "
    print(multiply 10 [1..5])
    putStrLn "\n3) Maior número da lista [1, 6, 610, 123, 1356, 134, 692, 9245, 10]:"
    print(maximumNum [1, 6, 610, 123, 1356, 134, 692, 9245, 10])
    putStrLn "\n4) Reverte os elementos da lista [1..50]:"
    print(reverseList [1..50])
    putStrLn "\n5) Triângulos:"
    print(isTriangle 4 7 12)
    print(isTriangle 5 5 5)
    print(isTriangle 5 1 5)
    print(isTriangle 5 9 10)
    putStrLn "\n6) Total dos produtos [(\"Brigadeiro\", 1), (\"Biscoito\", 15), (\"Chocolate\", 50), (\"Maça\", 10), (\"Pão\", 1)]:"
    print(cartTotal [("Brigadeiro", 1), ("Biscoito", 15), ("Chocolate", 50), ("Maça", 10), ("Pão", 1)])
    putStrLn "\n7) Total dos produtos [(\"Brigadeiro\", 1, 25), (\"Biscoito\", 15, 3), (\"Chocolate\", 50, 2), (\"Maça\", 10, 5), (\"Pão\", 1, 10)]:"
    print(cartTotal' [("Brigadeiro", 1, 25), ("Biscoito", 15, 3), ("Chocolate", 50, 2), ("Maça", 10, 5), ("Pão", 1, 10)])
    putStrLn "\n8) Total de cada produto [(\"Brigadeiro\", 1, 25), (\"Biscoito\", 15, 3), (\"Chocolate\", 50, 2), (\"Maça\", 10, 5), (\"Pão\", 1, 10)]:"
    print(cartFinance [("Brigadeiro", 1, 25), ("Biscoito", 15, 3), ("Chocolate", 50, 2), ("Maca", 10, 5), ("Pao", 1, 10)])

