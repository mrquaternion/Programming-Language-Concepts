-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- Reconnaissance du symbole "true"
s2l (Ssym "true") = Lbool True 

-- Reconnaissance du symbole "false"
s2l (Ssym "false") = Lbool False 

-- Gestion de la liste vide
s2l Snil = Lfob [] (Lvar "vide") 

-- Conversion d'une expression let
s2l (Snode (Ssym "let") [Ssym var, val, body]) = 
    Llet var (s2l val) (s2l body)

-- Conversion d'une expression fix
s2l (Snode (Ssym "fix") [bindings, body]) = 
    Lfix (bindsExtraction bindings) (s2l body)
    where
        bindsExtraction :: Sexp -> [(Var, Lexp)]
        bindsExtraction (Snode firstBind restBinds) = 
            map bindExtraction (firstBind : restBinds)
        bindsExtraction _ = 
            error "Structure de la liste de liaisons invalide dans fix."

        bindExtraction :: Sexp -> (Var, Lexp)
        -- Cas à 3 imbrications
        bindExtraction (Snode (Snode (Ssym var) [args]) [newBody]) =
            (var, Lfob (argsExtraction args) (s2l newBody)) 
        -- Cas à 2 imbrications
        bindExtraction (Snode (Ssym var) [newBody]) = 
            (var, (s2l newBody))
        bindExtraction _ = error "Structure de liaison invalide dans fix."

        argsExtraction :: Sexp -> [Var]
        argsExtraction Snil = []
        argsExtraction (Ssym arg) = [arg]
        argsExtraction (Snode arg rest) = argsExtraction arg 
                    ++ myMap argsExtraction rest
                        where
                            myMap :: (Sexp -> [Var]) -> [Sexp] -> [Var]
                            myMap _ [] = []
                            myMap f (arg2 : rest2) = f arg2 ++ myMap f rest2 
        argsExtraction _ = error "Structure d'arguments invalide dans fix."

-- Gestion des expressions fob
s2l (Snode (Ssym "fob") [vars, body]) = Lfob (vsExtraction vars) (s2l body)
    where
        vsExtraction :: Sexp -> [Var]
        vsExtraction Snil = [] -- Pas d'arguments
        vsExtraction (Snode hd tl) = vExtraction hd : vsExtraction' tl
        vsExtraction _ = error "Liste de variables invalide dans fob."

        vsExtraction' :: [Sexp] -> [Var]
        vsExtraction' [] = []
        vsExtraction' (x:xs) = vExtraction x : vsExtraction' xs

        vExtraction :: Sexp -> Var
        vExtraction (Ssym var) = var
        vExtraction _ = error "Variable invalide dans fob."

-- Conversion d'une expression conditionnelle if
s2l (Snode (Ssym "if") [cond, thenExp, elseExp]) = 
    Ltest (s2l cond) (s2l thenExp) (s2l elseExp)

-- Gestion des opérateurs et des appels de fonctions
s2l (Snode op args) = Lsend (s2l op) (map s2l args)
s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")

          in [("+", binop Vnum (+)),
              ("*", binop Vnum (*)),
              ("/", binop Vnum div),
              ("-", binop Vnum (-)),
              ("<", binop Vbool (<)),
              (">", binop Vbool (>)),
              ("≤", binop Vbool (<=)),
              ("≥", binop Vbool (>=)),
              ("=", binop Vbool (==)),
              ("true",  Vbool True),
              ("false", Vbool False)]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
eval _ (Lnum n) = Vnum n
eval _ (Lbool b) = Vbool b 

-- Source : 2e devoir
eval env (Lvar var) =
    case elookup env var of 
        Just v -> v
        Nothing -> error ("Variable " ++ var ++ " non définie.")
        where
            elookup :: VEnv -> Var -> Maybe Value
            elookup [] _ = Nothing
            elookup ((var', val') : restEnv) currVar
                | currVar == var' = Just val'
                | otherwise = elookup restEnv currVar

-- Évaluation d'une expression let
eval env (Llet var val body) =
    let newEnv = (var, eval env val) : env
    in eval newEnv body

-- Évaluation d'une expression fix pour les liaisons récursives
eval env (Lfix bindings body) =
    let newEnv = [(var, eval newEnv expr) | (var, expr) <- bindings] ++ env
    in eval newEnv body

-- Évaluation d'une fonction fob
eval env (Lfob params body) = Vfob env params body

-- Appel de fonction ou opérateur avec évaluation des arguments
eval env (Lsend func args) =
    case eval env func of
        Vfob closureEnv params body -> -- Pattern utilisé par Lfob
            if length params == length args
                then
                    let argVals = map (eval env) args
                        newEnv = myZip params argVals ++ closureEnv ++ env
                    in eval newEnv body
                else error ("Manque " 
                                ++ show (abs (length params - length args))
                                                ++ " arguments à la fonction.")
                    where
                        myZip :: [Var] -> [Value] -> [(Var, Value)]
                        myZip _ [] = []
                        myZip [] _ = []
                        myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
        Vbuiltin f -> f (map (eval env) args) -- Pattern utilisé par Lfix, Llet
        _ -> error "N'est pas une fonction."

-- Évaluation d'une expression conditionnelle if
eval env (Ltest cond thenExp elseExp) =
    case eval env cond of
        Vbool True  -> eval env thenExp
        Vbool False -> eval env elseExp
        _ -> error "La condition doit être un booléen."

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
