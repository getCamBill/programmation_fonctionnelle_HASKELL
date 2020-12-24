module Main where

import Graphics.Gloss

main :: IO()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (shenron a b !! (round t `mod` 20))

 
--dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
--dragonAnime a b t = Line (shenronOrdreBis a b (round 10 `mod` 20))

--type Point = (Float, Float)
--type Path = [Point]

pointAintercaler :: Point -> Point -> Point 
pointAintercaler a b = ((fst a +fst b)/2 + (snd b - snd a)/2, 
                       (snd a + snd b)/2 + (fst a - fst b)/2)

pasShenron :: Path -> Path
pasShenron [] = []
pasShenron [a] = [a]
pasShenron [a,b] = a:pointAintercaler a b: b :[]
pasShenron (a:b:c:xss) = (pasShenron [a,b] ++ [pointAintercaler c b] ++ (pasShenron (c:xss)))

shenron :: Point -> Point -> [Path]
shenron a b = iterate (pasShenron) xs
  where xs = pasShenron [a,b]

shenronOrdre :: Point -> Point -> Int -> Path
shenronOrdre a b 0 = pasShenron [a,b] 
shenronOrdre a b n = concat (take n (iterate (pasShenron) xs))
  where xs = pasShenron [a,b]

shenronOrdreBis :: Point -> Point -> Int -> Path
shenronOrdreBis a b 0 = [a,b]
shenronOrdreBis a b n = shenronOrdreBis a c (n-1) ++ shenronOrdreBis b c (n-1)
    where c = (pointAintercaler a b)
