import System.Random
import Data.Char
import Data.Map as Map
import Control.Monad
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Instances

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newnewGen) = random newGen
        (thirdCoin, _) = random newnewGen
    in (firstCoin, secondCoin, thirdCoin)

myRandoms :: (RandomGen g, Random a) => g -> [a]
myRandoms gen = 
    let (value, newGen) = random gen in value : myRandoms newGen

finiteRandoms :: (RandomGen g, Random a) => Int -> g-> ([a], g)
finiteRandoms 0 g = ([], g)
finiteRandoms n g =
    let (value, newGen) = random g
        (restValue, finalGen) = finiteRandoms (n-1) newGen
    in (value : restValue, finalGen)


cutRod :: Int -> Maybe Int
cutRod x = cutRodAux [0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30] x $ Map.fromList [(0, 0)]


cutRodAux :: [Int] -> Int -> Map.Map Int Int -> Maybe Int
cutRodAux p n r 
    | Map.lookup n r /= Nothing = Map.lookup n r
    | n == 0 = Just 0
    | otherwise = Map.lookup n $ Map.insert n maxE r 
        where maxE = maximum $ Prelude.map res [1..n] 
              res i = maybe 0 id ( cutRodAux p (n - i) r ) + (p !! i)



data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

type RoadSystem = [Section]

heathrowToLodon :: RoadSystem
heathrowToLodon = [Section 50 10 30, 
                    Section 5 90 20, 
                    Section 40 2 25, 
                    Section 10 8 0]

instance Show Label where
    show A = "A"
    show B = "B"
    show C = "C"

data Label = A | B | C 
type Path = [(Label, Int)] 

optimalPath :: RoadSystem -> (Path, Path)
optimalPath heathrowToLodon =
    Prelude.foldl roadStep ([(A, 0)], [(B, 0)]) heathrowToLodon

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let aToB = (C, a + c + (snd $ head pathA))
        bToA = (C, b + c + (snd $ head pathB))
        aToA = (A, a + (snd $ head pathA))
        bToB = (B, b + (snd $ head pathB))
        fastToA = if snd aToA >= snd bToA
            then bToA : pathB
            else aToA : pathA
        fastToB = if snd aToB >= snd bToB 
            then bToB :pathB
            else aToB : pathA
    in (fastToA, fastToB)


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
--groupsOf n xs = take n xs : groupsOf n $ drop n xs

positions :: Int -> [Int] -> [Int] 
positions a xs = [l | (l, r) <- zip [0.. length xs] xs, r == a]

aaa :: [a] -> [(a, a)]
aaa a = [x | x <- zip a $ tail a]

myRoll :: Int -> Int
myRoll n = length . (Prelude.filter) (\(a, b) -> a == b) . take n $ rPair
    where rPair = zip  (rStream 1) ( rStream 2)
          rStream n = randomRs (1, 3) $ mkStdGen n :: [Int]

myBetterRoll :: Int -> Int
myBetterRoll n = length . (Prelude.filter) (\(a, b) ->  a /= b ) . take n $ rPair
    where rPair = zip  (rStream 2) (rStream 1)
          rStream n = randomRs (1, 3) $ mkStdGen n :: [Int]


odd_even :: (Num a) => [a] -> [a]
odd_even [] = []
odd_even (x:[]) = [x]
odd_even (x:y:xs) = (2*x) : (4*y) : odd_even xs

safetail [x] = [x]
safetail (_ : xs) = xs

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = (sum . init $ factors num) == num




let2int :: Char -> Int
let2int c =ord c

int2let :: Int -> Char
int2let n =chr n 

shift :: Int -> Char -> Char
shift n c 
    | isLower c = int2let (mod (let2int c + n - ord 'a') 26 + ord 'a')
    | isUpper c = int2let (mod (let2int c + n - ord 'A') 26 + ord 'A') 
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

xs = 1 : [1 + x | x <- xs]


type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole ->Maybe Pole
landLeft n  (left, right)
    | abs ((left + n) - right) > 3 = Nothing
    | otherwise = Just (left + n, right)


landRight :: Birds -> Pole ->Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) > 3 = Nothing
    | otherwise = Just (left, right + n)

banana :: Pole -> Maybe Pole
banana _ = Nothing

type KnightPos = (Int, Int, [(Int, Int)])

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r, xs) = do
    guard (elem c [1..8] && elem r [1..8])
    (c', r', _) <- [(c+2, r-1, xs), (c+2, r+1, xs), (c-2, r-1, xs), (c-2, r+1, xs), 
                 (c+1, r-2, xs), (c+1, r+2, xs), (c-1, r-2, xs), (c-1, r+2, xs)]
    guard (elem c' [1..8] && elem r' [1..8])
    return (c', r', xs ++ [(c', r')])

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReadIn3 :: (Int, Int) -> (Int, Int) -> [(Int, Int, [(Int, Int)])]
canReadIn3 (a, b) (c, d) = Prelude.filter isIn res
    where res = in3 (a, b, [])
          isIn = (\(x, y, _) -> and [(x == c), (y == d)])


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with" ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show(mod a b)]
        gcd' b (mod a b)


addStuff :: Int -> Int
addStuff = (*2) >>= (\a -> (+10) >>= (\b -> return (a+b)))
