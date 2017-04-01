import Data.List

--1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x : []) = Just x
myLast (x : xs) = myLast xs

--2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Just x
myButLast [x, _] = Just x
myButLast (_ : xs) = myButLast xs

--3
elementAt :: (Eq a) => [a] -> Int -> Maybe a
elementAt [] n = Nothing
elementAt (x : xs) n
    | n <= 0 = Nothing
    | n == 1 = Just x
    | xs == [] = Nothing
    | otherwise = elementAt xs (n-1)

--4
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = myLength xs + 1

--5
myReverse :: (Eq a) => [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

--7
data NestedList a = Elem a | List [NestedList a]
caseSeven =  (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--8
caseEight = "aaaabccaadeeee"
compress :: (Eq a) => [a] -> [a]
compress = map head . group

--9
caseNight = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
pack :: (Eq a) => [a] -> [[a]]
pack xs = group $ foldl (\acc x -> acc ++ [x]) [] xs

--10
--same case as 9
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\ x -> (length x, head x))  $pack xs

--11
caseEleven = "aaaabccaadeeee"
data Eleven a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Eleven a]
encodeModified xs = map subEleven $ encode xs
    where subEleven (x, y) = if x == 1 then Single y else Multiple x y

--12
caseTwelve = [Multiple 4 'a', Single 'b', Multiple 2 'c', 
                Multiple 2 'a', Single 'd', Multiple 4 'e']
decodeModified :: [Eleven a] -> [a]
decodeModified xs = concatMap subTwelve xs
    where subTwelve (Single a) = [a]
          subTwelve (Multiple n a) = replicate n a

--13
--I don't know to do what

--14
fourteenCase = [1, 2, 3]
dupli :: [a] -> [a]
dupli xs = foldl (\acc x -> acc ++ [x] ++ [x]) [] xs

--15
fifteenCase = [1, 2, 3]
repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc x -> acc ++ replicate n x) [] xs

--16
sixteenCase = "abcdefghik"
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = subSixteen xs n
    where subSixteen :: [a] -> Int -> [a]
          subSixteen (x : xs) 1 = subSixteen xs n
          subSixteen (x : xs) k = x : subSixteen xs (k-1)
          subSixteen [] k = []

--17
seventeenCase = sixteenCase
split :: [a] -> Int -> [[a]]
split xs n = [(take n xs),  (drop n xs)]

--18
eighteenCase = "abcdefghijk"
slice :: [a] -> Int -> Int -> [a]
slice xs s e = take (e-s+1) $ drop (s-1) xs
