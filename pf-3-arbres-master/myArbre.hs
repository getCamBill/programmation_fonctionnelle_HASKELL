{-# LANGUAGE TupleSections #-}
import Test.QuickCheck ()
import Control.Concurrent (threadDelay)

data Couleur = R | N deriving (Show, Eq)
type ArbreRN = Arbre Couleur  

-- Q1
data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val) 
                      | Feuille -- ou Arbre Vide
                      deriving (Show, Eq) 
  
-- Q2
mapArbre :: (ca -> cb) -> (va -> vb) -> Arbre ca va -> Arbre cb vb 
mapArbre _ _ Feuille = Feuille
mapArbre fcl fv (Noeud c v treeG treeD) = Noeud (fcl c) (fv v) (mapArbre fcl fv treeG) (mapArbre fcl fv treeD)


-- Q3
--hauteur :: Arbre c v -> Int
--hauteur Feuille = 0
--hauteur (Noeud _ _ treeG treeD) = 1 +  max (hauteur treeG) (hauteur treeD)

-- Q3 
--taille :: Arbre c v -> Int
--taille Feuille = 0
--taille (Noeud _ _ treeG treeD) = 1 + (+) (hauteur treeG) (hauteur treeD)

-- Q4
dimension :: (Int -> Int -> Int) -> Arbre c v -> Int
dimension _ Feuille = 0
dimension f (Noeud _ _ tG tD) = 1 + f (dimension f tG) (dimension f tD)

taille :: Arbre c v -> Int
taille = dimension (+)

hauteur :: Arbre c v -> Int
hauteur = dimension max

-- Q5
peigneGauche :: [(c,v)] -> Arbre c v
peigneGauche [] = Feuille
peigneGauche (x:xs) = Noeud c v (peigneGauche xs) Feuille 
  where c = fst x
        v = snd x

-- Q6 
prop_hauteurPeigne :: [(c, v)] -> Bool
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

-- Q7
--
prop_tailleArbre :: [(c, v)] -> Bool
prop_tailleArbre xs = length xs == taille (peigneGauche xs)


-- prop_mapArbre fc fv (Noeud c v treeG treeD)  = length xs == hauteur (peigneGauche xs)
--

-- Q8 
estComplet :: Arbre c v -> Bool
estComplet Feuille = False
estComplet (Noeud _ _ Feuille Feuille) = True
estComplet (Noeud _ _ treeG treeD) = estComplet treeG && estComplet treeD && hauteur treeG == hauteur treeD 
--λ> estComplet (Noeud () [] Feuille Feuille) 
--True

--λ> estComplet (Noeud () [] (Noeud () [] Feuille Feuille) Feuille) 
--False

--λ> estComplet (Noeud () [] (Noeud () [] Feuille Feuille) (Noeud () [] Feuille Feuille)) 
--True

-- Q9
estComplet' :: Arbre c v -> Bool
estComplet' tree = taille tree + (2^size) == fx size
    where size = hauteur tree

fx :: Int  -> Int
fx 0 = 1
fx n = 2^n + fx (n-1)

-- Q10 ET 11
-- noeud 2^n -1
-- Noeud coul val (Arbre coul val) (Arbre coul val) 
complet :: Int -> [(c, v)] -> Arbre c v
complet _ [] = Feuille 
complet 0 _ = Feuille
complet n (x:xs) 
      | n > 0 =  Noeud c v (complet (n-1) (fst (split xs))) (complet (n-1) (snd (split xs)))
      where c = fst x
            v = snd x
complet _ _ = Feuille

split :: [(c, v)] -> ([(c, v)], [(c, v)])
split xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

--Q12
returner :: a -> a
returner a = a

repeat' :: a -> [a]
repeat' = iterate returner 

--Q13
suiteTupleList :: String -> [((),Char)]
suiteTupleList = map ((), ) 
--suiteTupleList [] = []
--suiteTupleList (x:xs) = ((),x) : (suiteTupleList xs)

--Q14
aplatit :: Arbre c a -> [(c, a)]
aplatit = f 
    where f Feuille = []
          f (Noeud c a treeG treeD) = (c,a) : f treeG ++ f treeD
-- let c4 = suiteTupleList ['a'..'o']
-- let complet4 = complet 4 c4
-- map snd (aplatit complet4) == "abcdefghijklmno" -> True

--Q15
element :: Eq a => a -> Arbre c a -> Bool
element x arbre = x `elem` map snd (aplatit arbre)

--Q16 call fa
noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud _ f t = f (snd t) ++ fc (fst t)

fa :: Char -> String
fa = show 

fc :: c -> String
fc _ = " [color=red, fontcolor=red]"

fC :: Couleur -> String
fC N = " [color=black, fontcolor=black]"
fC R = " [color=red, fontcolor=red]"

-- Q17
arcs :: Arbre c a -> [(a,a)]
arcs Feuille = []
arcs (Noeud _ a treeG treeD) = go a treeG ++ go a treeD  ++ arcs treeG ++ arcs treeD
      where go _ Feuille = []
            go s1 (Noeud _ s2  _ _ ) = [(s1, s2)]


-- Q18 d -> b call fa 
arc ::Ord a => (a -> String) -> (a,a) -> String
arc f aa = f (fst aa) ++ " -> " ++ f (snd aa) 

fonctionQuiTue :: (c->String) -> (a->String) -> (c,a) -> String
fonctionQuiTue f1 f2 askHell = f2 (snd askHell) ++ " " ++ f1 (fst askHell)

-- Q19               fc                  arc
dotise :: String -> (Couleur->String) -> (Char->String) -> ArbreRN Char -> String
dotise titre f1 f2 tree = unlines
 ["digraph " ++ show titre ++ "{",
 "node [fontname=\"DejaVu-Sans\", shape=circle]\"", 
 "/* Liste des nœuds */",
--   unlines (map (\x -> fonctionQuiTue f1 f2 x) (aplatit tree)),
 unlines (map (fonctionQuiTue f1 f2) (aplatit tree)),
 "/* Liste des arcs */",  -- [(c,a)] -> "'a' [color=red, fontcolor=red]"
--  unlines (map (\x -> arc f2 x) (arcs tree)), "",
 unlines (map (arc f2) (arcs tree)), "", 
 "}"]

-- si arbre non équilibré
elementR' :: Ord a => Arbre c a -> a -> Maybe a
elementR' tree a
       | element a tree = Just a
       | otherwise = Nothing

-- si arbre équilibré
elementR :: Ord t => Arbre coul t -> t -> Maybe t
elementR Feuille _ = Nothing
elementR (Noeud _ t treeG treeD) s
                              | s == t    = Just s
                              | s < t     = elementR treeG s
                              | otherwise = elementR treeD s

-- Rappel
-- data Couleur = R | N deriving Show 
-- type ArbreRN a = Arbre Couleur Char

-- -- Q22
equilibre :: ArbreRN a -> ArbreRN a
equilibre (Noeud N z (Noeud R y (Noeud R x a b) c) d) =  Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N z (Noeud R x a (Noeud R y b c)) d) =  Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N x a (Noeud R y b (Noeud R z c d))) =  Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud N x a (Noeud R z (Noeud R y b c) d)) =  Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre treeEqui                                    =  treeEqui 


doBlack :: ArbreRN a -> ArbreRN a
doBlack Feuille = Feuille
doBlack (Noeud _ x a b) = Noeud N x a b

-- Q23
insertionRN :: Ord a => a -> ArbreRN a -> ArbreRN a
insertionRN x Feuille = Noeud R x Feuille Feuille
insertionRN x (Noeud c y tg td) 
      | x < y = equilibre (Noeud c y (insertionRN x tg) td)
      | otherwise = equilibre (Noeud c y tg (insertionRN x td))

insertion :: Ord a => ArbreRN a -> a -> ArbreRN a
insertion tree a = doBlack (insertionRN a tree)


-- Q24
createArbreRN :: String -> ArbreRN Char
createArbreRN = foldl insertion Feuille

 -- Tous les nœuds ont 2 enfants. 
 -- Ce sont d'autres nœuds ou des feuilles NIL, 
 -- qui ne possèdent pas de valeur et qui sont 
 -- les seuls nœuds sans enfants. Leur couleur est toujours
 --  noire et rentre donc en compte lors du calcul de la hauteur noire.
nbNoeudNoir :: ArbreRN a -> [Int]
nbNoeudNoir Feuille = [0]
nbNoeudNoir (Noeud R _ tg _) = nbNoeudNoir tg
nbNoeudNoir (Noeud N _ tg _) = 1 : nbNoeudNoir tg

verify :: ArbreRN a -> Bool
verify Feuille = True
verify (Noeud c _ left right)
  | c == N = verify left && verify right
  | col left == c = False
  | col right == c = False
  | otherwise = verify left && verify right

col :: ArbreRN a -> Couleur
col Feuille = N
col (Noeud c _ _ _) = c

-- Le chemin de la racine à n'importe quelle feuille
-- contient le même nombre de nœuds noirs. 
-- On peut appeler ce nombre de nœuds noirs la hauteur noire.
hauteurBlack :: ArbreRN a -> Int
hauteurBlack Feuille = 0
hauteurBlack (Noeud c _ t1 t2)  
      | hauteurBlack t1 /= hauteurBlack t2 = -1
      | c == N = 1 + hauteurBlack t1 
      | otherwise = hauteurBlack t1

-- P1 La racine est noire.
prop_ArbreRN_BlackRoot :: String -> Bool
prop_ArbreRN_BlackRoot [] = True
prop_ArbreRN_BlackRoot xs = fst(head(aplatit (createArbreRN xs))) == N

-- P2 Un nœud rouge n’a pas de fils rouge./ Si un noeud est rouge alors ses deux fils sont noirs.
prop_ArbreRN_NodesBlackTwoRed :: String -> Bool 
prop_ArbreRN_NodesBlackTwoRed [] = True
prop_ArbreRN_NodesBlackTwoRed xs = verify (createArbreRN xs)

-- P3 Tous les chemins de la racine à une feuille ont le même nombre de nœuds noirs
prop_ArbreRN_nbBlackNodes :: String -> Bool
prop_ArbreRN_nbBlackNodes xs = hauteurBlack(createArbreRN xs) /= -1


-- Q25
type LaForce = Bool 

(===) :: Ord jedi => ArbreRN jedi -> ArbreRN jedi -> LaForce
tree1 === tree2 = nbNoeudNoir tree1 == nbNoeudNoir tree2 && hauteurBlack tree1 == hauteurBlack tree2

(~==) :: Ord jedi => ArbreRN jedi -> ArbreRN jedi -> LaForce
tree1 ~== tree2 = tree1 === tree2 && yoda tree1 tree2

vador :: Ord jedi => ArbreRN jedi -> [jedi]
vador tree = map snd (aplatit tree)

yoda :: Ord jedi => ArbreRN jedi -> ArbreRN jedi -> LaForce
yoda t1 t2 =  all (==True) (map (\x -> x `elem` vador t1) (vador t2))


-- Q26
--dotise :: Show a => String -> (c->String) -> (a->String) -> Arbre c a -> String
arbresDot :: String -> [String]
arbresDot xs = [dotise "arbre" fC fa (createArbreRN x) | x <- reverse (mm xs (length xs))]

mm :: String -> Int -> [String]
mm xs n 
      | n > 0 = take n xs : mm xs (n-1)
      |otherwise = []

main :: IO ()
main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"