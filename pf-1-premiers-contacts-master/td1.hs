import Test.QuickCheck()
import Graphics.GL()
--import Data.Tuple
--TP1
--
--PArtie 1

--
-- 3  
sommedeXaY :: Int -> Int -> Int
--sommedeXaY _ _ = error "Aucun argument renseigné"
sommedeXaY x y = sum [x..y]
-- 4 
somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = x + somme xs

prop_somme :: [Int] -> Bool
prop_somme xs = somme xs == sum xs


-- 5
myLast :: [a] -> a
myLast [] = error "liste vide"
myLast xs = (take 1 (reverse xs)) !! 0
--myLast xs = head (reverse xs)
--myLast xs = (reverse xs) !! 0
--myLast xs = (drop (length(xs)-1) xs) !! 0
--myLast xs = (reverse(tail xs)) !! 0

prop_MyLast :: Eq a => [a] -> Bool
prop_MyLast xs = myLast xs == last xs


myInit :: [a] -> [a]
myInit [] = error "liste vide"
myInit xs = take (length(xs) - 1) xs
--myInit xs = reverse(tail(reverse xs))

prop_myInit :: Eq a => [a] -> Bool 
prop_myInit xs = myInit xs == init xs


-- 6
(!!!) :: [a] -> Int -> a
[] !!! _ = error "liste vide"
xs !!! i 
      | i < 0 = error "index negatif"
      | (length(xs)-1) <= i = error "index too large"
      | otherwise =  head (drop(length(xs)-i-1) xs)

prop_FiByIn :: Eq a => [a] -> Int -> Bool 
prop_FiByIn xs i = xs !!! i == xs !! i
--prop_FiByIn l i = i<length l ==> !!! l i == l !! i
  --where types = l::[Int]

(+++) :: [a] -> [a] ->  [a]
[] +++ ys = ys
xs +++ [] = xs
(x:xs) +++ ys = x: (xs +++ ys)

prop_Plusx3 :: Eq a => [a] -> [a] -> Bool 
prop_Plusx3 xs ys = xs +++ ys == xs ++ ys


myConcat :: [[a]] ->  [a]
myConcat [] = []
myConcat (xs:xss) = xs +++ myConcat xss

prop_Concat :: Eq a => [[a]] -> Bool 
prop_Concat xs = myConcat xs == concat xs


myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f(x)):(myMap f xs) 
--myMap f xs = [f x | x <- xs]
prop_MyMap :: Eq a => (a -> a) -> [a] -> Bool 
prop_MyMap f xs = myMap f xs == map f xs 

-- 7 
-- x = (!!) liste pas compris

-- 8
longeur' :: [a] -> Int
longeur' [] = 0
longeur' xs = somme (myMap (\_ -> 1) xs)

prop_longueur :: [a] -> Bool
prop_longueur xs = longeur' xs == length xs 

-- 9
-- Recursive Ne marche pas

fonction :: (a -> a) -> a -> Int -> [a]
--fonction f x n = take n (iterate f x) 
fonction _ x 0 = [x]
fonction f x n =  x : fonction f (f x) (n-1)

-- Avec iterate & take

--fonction :: (a -> a) -> a -> Int -> [a]
--fonction f x n = 
-- 10



-- //////////////////////////////////////
--    PArtie 2 

alterne :: [a] -> [a]
alterne [] = []
alterne xs = [x | (x, i) <- zip xs [1..] ,i `mod` 2 /= 0 ]

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ [] _ = []
combine _ _ [] = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys

-- Traingle de Pascalou
--
--pasPascalou :: [Integer] -> [Integer]
--pasPascalou [] = []
--pasPascalou xs = combine (+) (xs++[0]) (0:xs) 

--pascalou :: [[Integer]]
--pascalou = iterate pasPascalou [1]
--
--
type Point = (Float, Float)
type Path = [Point]

pointAintercaler :: Point -> Point -> Point
pointAintercaler a b = ((fst a +fst b)/2 + (snd b - snd a)/2, 
                       (snd a + snd b)/2 + (fst a - fst b)/2)

pasShenron :: Path -> Path
pasShenron [] = []
pasShenron [a] = [a]
pasShenron [a,b] = a:pointAintercaler a b: b :[]
pasShenron (a:b:c:xss) = (pasShenron [a,b] ++ [pointAintercaler c b] ++ (pasShenron (c:xss)))


--shenronOrdre :: Point -> Point -> Int -> Path
--shenronOrdre a b 0 = pasShenron [a,b] 
--shenronOrdre a b n = 
--
--somme :: Int -> Int
--somme n = sum[1..n]

--longueur :: [a] -> Int
--longueur [] = 0
--longueur (_:xs) = 1 + longueur xs

--longueur' :: [a] -> Int
--longueur' xs = case xs of
  --              [] -> 0
    --            _:xs' -> 1 + longueur' xs'

--liste_sommes :: Num a => [a] -> [a] -> [a]
--liste_sommes []     []     = []
--liste_sommes (x:xs) (y:ys) = x+y : liste_sommes xs ys

--sommeListe :: [Num a] => [a] -> a
--swap' (x, y) = (y, x)
--double a = a * 2
--palind xs = reverse xs == xs
--twice f x = f (f x)



