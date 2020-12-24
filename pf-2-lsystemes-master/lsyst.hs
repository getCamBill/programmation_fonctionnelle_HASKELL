import Graphics.Gloss

type Symbole = Char
type Mot = [Symbole]
type Axiome = Mot
type Regles = Symbole -> Mot 
type LSysteme = [Mot]

-----------------------------
-- L-sytème
-----------------------------

regles :: Symbole -> Mot -- ou Regle mais plus clair
regles s
  |s == 'F' = ['F','-','F','+','+','F','-','F']
  |s == '+' = ['+']
  |otherwise = ['-']
-- Q1
motSuivant :: Regles -> Mot -> Mot
motSuivant _ [] = []
motSuivant rules (s:mot) = rules s ++ motSuivant rules mot 

motSuivant' :: Regles -> Mot -> Mot
motSuivant' rules mot = concat (map rules mot)

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' rules mot = concat [rules x | x <- mot]

-- Q3
lsysteme :: Axiome -> Regles -> LSysteme 
lsysteme [] _ = []
lsysteme ax rules = motSuivant rules ax : lsysteme axs rules
  where axs = motSuivant rules ax

--lsysteme ax rules = motSuivant rules ax : iterate (lsysteme axs rules)
--  where axs = motSuivant rules ax

---------------------------
-- Tortue
--------------------------

type EtatLeonardo = (Point, Float)

type Config = (EtatLeonardo -- Etat init de la tortue
              ,Float        -- Longueur init d'un pas
              ,Float        -- Facteur d'échelle
              ,Float        -- Angle pour les rotations de la tortue
              , [Symbole])  -- == Mot liste des symbole compris par leonardo

type EtatOeuvre = (EtatLeonardo, Path)

-- Q4
etatInit :: Config -> EtatLeonardo 
etatInit (a,_,_,_,_) = a
longueurPas :: Config -> Float
longueurPas (_,a,_,_,_) = a
facteurEchelle :: Config -> Float
facteurEchelle (_,_,a,_,_) = a
angle :: Config -> Float
angle (_,_,_,a,_) = a
symboleTortue :: Config -> [Symbole]
symboleTortue (_,_,_,_,a) = a

-- Q5
avance :: Config -> EtatLeonardo -> EtatLeonardo   
avance conf ((x,y), cap) = ((x+d*cos(cap), y+ d* sin(cap)),cap )
  where d = longueurPas conf 

-- Q6
tourneADroite :: Config -> EtatLeonardo -> EtatLeonardo 
tourneADroite conf state  = (fst state, snd state - oldAngle) 
  where oldAngle = angle conf 

tourneAGauche :: Config -> EtatLeonardo -> EtatLeonardo
tourneAGauche conf state  = (fst state, snd state + oldAngle)
  where oldAngle = angle conf  

-- Q7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue conf mot = [x | x <- mot, elem x symbole]
  where symbole = symboleTortue conf 

-- Q8
goodFunc :: Symbole -> (Config -> EtatLeonardo -> EtatLeonardo)
goodFunc sym 
        |sym == 'F' = avance
        |sym == '+' = tourneAGauche 
        |otherwise = tourneADroite 

interpreteSymbole :: Config -> ((Point, Float) ,[Point]) -> Symbole -> EtatOeuvre 
interpreteSymbole conf soa sym = (newEtatLeo, newPath)
  where newEtatLeo = (goodFunc sym) conf (fst soa) -- ex : avance conf etatLeo 
        newPath = fst newEtatLeo: snd soa
        
-- Q9 
-- ajout en tête pour éviter de parcourir la liste entièrement

-- Q10 

listePath :: Config -> EtatOeuvre -> Mot -> EtatOeuvre 
listePath conf statOfArt xs = foldl (interpreteSymbole conf) statOfArt  xs 

interpreteMot :: Config -> Mot -> Picture
interpreteMot conf mot = Line (snd (listePath conf (etatInit conf ,[fst (etatInit conf)]) mot))

--dessin :: Picture 
--dessin = interpreteMot (((-150, 0),0),100,1,pi/3,"F+-") "F+F--F+F"
--main :: IO()
--main = display (InWindow "L-Syst" (1000,1000) (0,0)) white dessin 

leonardo :: LSysteme
leonardo = lsysteme "F" regles 

lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lsys conf t  = interpreteMot conf (lsys !! (round t `mod` 8)) 

--enieme :: Integer
--enieme = round instant `mod` 8
 
main:: IO()
main = animate (InWindow "L-Syst" (1000,1000) (0,0)) white hilbertAnime 
--main = animate (InWindow "L-Syst" (1000,1000) (0,0)) white 
  --     (lsystemeAnime leonardo (((-150, 0),0),100,1,pi/3,"F+-"))

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


---------------------------
-- Tortue volante
--------------------------

