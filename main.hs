lenList :: [Int] -> Int
lenList [] = 0
lenList (v:vals) = 1 + lenList vals

double :: [Int] -> [Int]
double [] = []
double (v:vals) = (v * 2):(double vals)

pairs :: [Int] -> [Int]
pairs [] = []
pairs (v:vals)
    | mod v 2 == 0 = v : (pairs vals)
    | otherwise = pairs vals

firstN :: Int -> [Int] -> [Int]
firstN 0 _ = []
firstN n (v:vals) = v : (firstN (n - 1) vals)

myFilter :: (Int -> Bool) -> [Int] -> [Int]
myFilter _ [] = []
myFilter f (l:list)
    | f l = l : myFilter f list
    | otherwise = myFilter f list

indexOf :: Int -> [Int] -> Maybe Int
indexOf _ [] = Nothing
indexOf el (v:list)
    | el /= v = case indexOf el list of
        Just n -> Just (n + 1)
        Nothing -> Nothing
    | otherwise = Just 0

main :: IO ()
main =  do
    putStrLn "Length of a list:"
    print(lenList [1..100])
    putStrLn "Double the elements of a list:"
    print(double [1..10])
    putStrLn "Get the pair elements of a list:"
    print(pairs [0..50])
    putStrLn "First elements of a list:"
    print(firstN 10 [1..100])
    putStrLn "A custom filter function for a list:"
    putStr   "\tElements greater than 5:\n\t\t"
    print(myFilter (\x -> x > 5) [0..10])
    putStr   "\tEven numbers:\n\t\t"
    print(myFilter (\x -> mod x 2 == 0) [0..10])
    putStrLn "Index of a element on the list:"
    -- TODO: convert Just to normal value
    print(indexOf 9 [0,5,7,2,4,6,8,1,9,3])
