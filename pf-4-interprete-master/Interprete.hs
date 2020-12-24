import Parser
import Data.Char
import Data.List
import Data.Maybe
-- import Data.Either
import System.IO


-- ////////////     RAPPEL      \\\\\\\\\\\\\\\\\\

        -- Expression	        Représentation
        --     a	                Var "a"
        --     \x -> x	            Lam "x" (Var "x")
        --     True	                Lit (Bool True)
        --     not True	            App (Var "not") (Lit (Bool True))
        --     add	                Var "add"
        --     add 12	            App (Var "add") (Lit (Entier 12))
        --     add 12 34	        App (App (Var "add") (Lit (Entier 12))) (Lit (Entier 34))


type Nom = String

data Expression = Lam Nom Expression        -- la fonction qui associe à une variable (Nom) un corps (Expression) 
                | App Expression Expression -- l’application d’une fonction à son argument, tous les deux donnés comme des Expressions 
                | Var Nom                   -- une variable 
                | Lit Litteral              -- un littéral, c’est-à-dire un entier ou un booléen, dans notre langage minimal.
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

-- **** Analyse proprement dite **** --

-- Q1
space :: Parser Char
space = carQuand isSpace

espacesP :: Parser ()
espacesP = do 
            _ <- many $ carQuand (`elem` " \t") 
            return ()
-- espacesP = car ' '
-- espacesP = many (car ' ') >> pure ()

-- Q2
nom :: Parser Char
nom = carQuand isAlpha 

nomP :: Parser String
nomP = nom >>=  \c ->
      (nomP >>= \cs -> espacesP >> pure (c:cs))
      <|> (espacesP >> pure [c])
      
-- Q3
varP :: Parser Expression
varP = nomP >>= \n -> pure (Var n)
-- varP = nomP >>= (pure . Var) -- hlint 

-- Q4
applique :: [Expression] -> Expression
applique = foldl1' App 

-- Q5 & Q7
exprP :: Parser Expression 
exprP = booleenP <|> lambdaP <|>  nombreP 
      <|> varP <|>  exprParentheseeP
        

yoda :: Parser [Expression]
yoda = exprP >>= \c -> 
            (yoda >>= \cs -> pure (c:cs))
      <|> pure [c]

exprsP :: Parser Expression 
exprsP = yoda >>= \l -> pure (applique l)

-- Q6
-- \    x   e 
-- Lam "x" exprsP e
-- \ x -> e où e est parsé pas exrpsP
lambda :: Parser String
lambda = chaine "λ" <|> chaine "\\"

lambdaP :: Parser Expression
lambdaP = lambda >> espacesP >> 
      nomP >>= \c -> 
            ( chaine "->" >> espacesP ) >> 
             exprsP >>= \e ->  pure (Lam c e)
 
 -- Q8
sColuccioO :: Parser Char
sColuccioO = car '(' 
sColuccioF :: Parser Char
sColuccioF = car ')'

exprParentheseeP :: Parser Expression
exprParentheseeP = sColuccioO >> espacesP >>
       exprsP >>= \y -> sColuccioF >> espacesP >>
            pure y

-- exprParentheseeP :: Parser Expression
-- exprParentheseeP = do _ <- car '('
--                       espacesP
--                       e <- exprP
--                       _ <- car ')'
--                       espacesP
--                       return e

-- Q9
nombreP :: Parser Expression
nombreP = do
  nb <- some $ carQuand isDigit
  espacesP
  pure $ Lit $ Entier $ read nb

-- Q10
bool :: Parser String
bool = chaine "True" <|> chaine "False"

booleenP :: Parser Expression
booleenP = bool >>= \b -> espacesP >>
       pure (Lit (Bool(read b || False)))

-- Q11
expressionP :: Parser Expression
expressionP = espacesP >> exprsP

-- Q12
res :: Resultat a -> a
res (Just (r, _)) = r
res _             = error "Erreur d’analyse syntaxique"

ras :: String -> Expression                              
ras xs = res (runParser expressionP xs)

-- **** Interprétation **** --


-- type Nom = String

-- data Expression = Lam Nom Expression        -- la fonction qui associe à une variable (Nom) un corps (Expression) 
--                 | App Expression Expression -- l’application d’une fonction à son argument, tous les deux donnés comme des Expressions 
--                 | Var Nom                   -- une variable 
--                 | Lit Litteral              -- un littéral, c’est-à-dire un entier ou un booléen, dans notre langage minimal.
--                 deriving (Show,Eq)

-- data Litteral = Entier Integer
--               | Bool   Bool
--               deriving (Show,Eq)

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)
             
-- Q13/14

instance Show ValeurA where
    show (VFonctionA _) = "\\" -- "λ"
    show (VLitteralA (Entier x)) = show x
    show (VLitteralA (Bool x)) = show x

-- Q15
type Environnement a = [(Nom, a)]

pika:: ValeurA -> ValeurA
pika (VLitteralA (Entier x)) = VLitteralA (Entier x)
pika (VLitteralA (Bool x)) = VLitteralA (Bool x)
pika (VFonctionA _) = error "poser la question sur comment gérer ce pattern"

interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit l) = VLitteralA l
interpreteA env (Var x) = fromJust (lookup x env) 
-- interpreteA [("x", VLitteralA (Entier 45))] (ras "(λx -> x) 67")
interpreteA env (Lam x e) = VFonctionA (\v -> interpreteA ((x,v):env) e)
interpreteA env (App ef ea) = let VFonctionA f = interpreteA env ef
                                  a            = interpreteA env ea
                              in  f a

--Q16
-- [("neg", negA)] (ras "neg 12")
nigga :: ValeurA -> ValeurA
nigga (VLitteralA (Entier x)) = VLitteralA (Entier (-x))
nigga _ = error "Non-exhaustive patterns in lambda"

negA :: ValeurA
negA = VFonctionA nigga 

-- Q17
-- [("neg", negA)] (ras "add 24 12")
addA :: ValeurA
addA = VFonctionA gaga 
      where 
            gaga :: ValeurA -> ValeurA
            gaga (VLitteralA (Entier e1)) = VFonctionA gogo
                  where 
                        gogo :: ValeurA -> ValeurA
                        gogo (VLitteralA (Entier e2)) = VLitteralA (Entier ((+) e1 e2))
                        gogo _ = error "Non-exhaustive patterns in lambda"
            gaga _ = error "Non-exhaustive patterns in lambda"

-- Q18
releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA f = VFonctionA gaga 
      where 
            gaga :: ValeurA -> ValeurA
            gaga (VLitteralA (Entier e1)) = VFonctionA gogo
                  where 
                        gogo :: ValeurA -> ValeurA
                        gogo (VLitteralA (Entier e2)) = VLitteralA (Entier (f e1 e2))
                        gogo _ = error "Non-exhaustive patterns in lambda"
            gaga _ = error "Non-exhaustive patterns in lambda"
      

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if", ifthenelseA) ]

-- interpreteA envA (ras "quot (mult 12 4) 5")

-- Q19
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA gaga 
      where 
            gaga :: ValeurA -> ValeurA
            gaga (VLitteralA (Bool b)) = VFonctionA gogo
                  where 
                        gogo :: ValeurA -> ValeurA
                        gogo (VLitteralA (Entier e1)) = VFonctionA gugu 
                              where 
                                    gugu :: ValeurA -> ValeurA
                                    gugu (VLitteralA (Entier e2)) = if b then VLitteralA (Entier e1) else VLitteralA (Entier e2)
                                    gugu _ = error "Non-exhaustive patterns in lambda"
                        gogo _ = error "Non-exhaustive patterns in lambda"
            gaga _ = error "Non-exhaustive patterns in lambda"

-- Q20

                                                  
main :: IO ()
main = do
    putStr "minilang> "
    hFlush stdout
    entree <- getLine -- on recup tte la chaine
    print (interpreteA envA (ras entree))
    hFlush stdout
    main


-- data Either a b = Left a
--                 | Right b

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)
-- Q21
instance Show ValeurB where
    show (VLitteralB (Entier x)) = show x
    show (VLitteralB (Bool x)) = show x
    show (VFonctionB _) = "\\"
    

-- Q22
interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB env (Var x) = 
                  case lookup x env of
                        Just (VLitteralB l)     -> Right (VLitteralB l)
                        Just (VFonctionB l)     -> Right (VFonctionB l)
                        Nothing                 -> Left ("La variable " ++ x ++ " n'est pas definie")
interpreteB _ (Lit l) = Right (VLitteralB l)
interpreteB env (Lam x e) = Right(VFonctionB (\v -> interpreteB ((x,v):env) e))
interpreteB env (App ef ea) = let vf = interpreteB env ef
                                  va = interpreteB env ea
                              in  case vf of
                                    Right (VFonctionB f) -> case  va of
                                                                  Right a -> f a
                                                                  Left a -> Left a
                                    Right v -> Left (show v ++ " n'est pas une fonction, application impossible")
                                    Left e -> Left e


-- Q23
addB :: ValeurB
addB = VFonctionB gaga 
      where 
            gaga :: ValeurB -> ErrValB
            gaga (VLitteralB (Entier e1)) = Right (VFonctionB gogo)
                  where 
                        gogo :: ValeurB -> ErrValB
                        gogo (VLitteralB (Entier e2)) = Right (VLitteralB (Entier ((+) e1 e2)))
                        gogo v = Left (show v ++ " n'est pas un entier")
            gaga v = Left (show v ++ " n'est pas un entier")

-- Q24
quotB :: ValeurB
quotB = VFonctionB gaga 
      where 
            gaga :: ValeurB -> ErrValB
            gaga (VLitteralB (Entier 0)) = Left "division par zero"
            gaga (VLitteralB (Entier e1)) = Right (VFonctionB gogo)
                  where 
                        gogo :: ValeurB -> ErrValB
                        gogo (VLitteralB (Entier 0)) = Left "division par zero"
                        gogo (VLitteralB (Entier e2)) = Right (VLitteralB (Entier (quot e1 e2)))
                        gogo v = Left (show v ++ " n'est pas un entier")
            gaga v = Left (show v ++ " n'est pas un entier")

-- Q25 
data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

instance Show ValeurC where
    show (VLitteralC (Entier x)) = show x
    show (VLitteralC (Bool x)) = show x
    show (VFonctionC _) = "\\"

-- Q26
interpreteC :: Environnement ValeurC -> Expression -> (Trace, ValeurC)
interpreteC _ (Lit l) = ("", VLitteralC l)
interpreteC env (Var x) = ("", fromJust (lookup x env) )
interpreteC env (Lam x e) = ("", VFonctionC (\v -> interpreteC ((x,v):env) e))

interpreteC env (App ef ea) = let VFonctionC f = snd (interpreteC env ef)
                                  a            = snd (interpreteC env ea)
                              in  f a

-- Q27 
-- pingC :: ValeurC

-- ghci> interpreteC [] (ras "1")
-- ("",1)
-- ghci> interpreteC [] (ras "(λx -> x) 1")
-- (".",1)
-- ghci> interpreteC [] (ras "(λx -> λy -> x) 1 2")
-- ("..",1)
-- ghci> interpreteC [] (ras "(λx -> x x)(λx -> x x)")
-- ("............................................^C..Interrupted.
