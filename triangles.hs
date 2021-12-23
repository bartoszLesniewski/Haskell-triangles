module Main where

-- Zwraca długość listy
myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

-- Zwraca pierwszy element listy
myHead :: [Int] -> Int
myHead [] = error "Empty lsit"
myHead(x:xs) = x

-- Zwraca pierwszy lub drugi element krotki w zależności od przekazanego argumentu
getNthElement :: Int -> (Int, Int) -> Int
getNthElement n (a, b)  =
  if n == 1 then a else b

-- Znajduje maksymalną wartość (ilość wystąpień) w liście krotek
findMax :: [(Int, Int)] -> Int 
findMax [] = error "Empty list"
findMax [(a, b)] = b
findMax (x:xs)
  | getNthElement 2 x > maxXs = getNthElement 2 x
  | otherwise = maxXs
  where maxXs = findMax xs

-- Znajduje wszystkie wartości o maksymalnej liczbie wystąpień
findAllMax :: [(Int, Int)] -> [(Int, Int)]
findAllMax x = filter (\ (a, b) -> b == max) x
  where max = findMax x

-- Zwraca tylko unikalne wartości wśród pierwszych elementów listy krotek
getUniqueValues :: [(Int, Int)] -> [Int]
getUniqueValues[] = []
getUniqueValues[(a, b)] = [a]
getUniqueValues(x:xs) = getNthElement 1 x : getUniqueValues(filter (/= x) xs)

-- Zwraca listę różnych trójek a, b, c, które spełniają warunki 
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles x = [(a, b, c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c <= x]

-- Zwraca listę zawierającą sumy boków trójkątów
sumOfSides :: [(Int, Int, Int)] -> [Int]
sumOfSides = map (\ (a, b, c) -> a + b + c)

-- Zwraca listę zawierającą liczby wystąpień każdej z sum
countOccurences :: [Int] -> [Int]
countOccurences x = map (\ a -> myLength (filter (== a) x)) x

-- Tworzy listę par (x, y), gdzie x oznacza sumę boków trójąta, a y liczbę wystąpień tej sumy
makePairs :: [(Int, Int, Int)] -> [(Int, Int)]
makePairs x = zip sum (countOccurences sum)
  where sum = sumOfSides x

-- Zwraca ostaetczny wynik
getResult :: Int -> [Int]
getResult x = getUniqueValues $ findAllMax $ makePairs(rightTriangles x)

main :: IO ()
main = do
  print "Podaj n:"
  n <- readLn
  print(getResult(n :: Int))
  