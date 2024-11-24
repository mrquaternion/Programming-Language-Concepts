-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
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
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
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

data Type = Terror String        -- Utilisé quand le type n'est pas connu.
          | Tnum                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tfob [Type] Type     -- Type des fobjets.
          deriving (Show, Eq)

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [(Var, Type)] Lexp -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

s2list :: Sexp -> [Sexp]
s2list Snil = []
s2list (Snode se1 ses) = se1 : ses
s2list se = error ("Pas une liste: " ++ showSexp se)

svar2lvar :: Sexp -> Var
svar2lvar (Ssym v) = v
svar2lvar se = error ("Pas un symbole: " ++ showSexp se)

s2type :: Sexp -> Type
s2type (Ssym "Num") = Tnum
s2type (Ssym "Bool") = Tbool
s2type (Snode (Ssym "fob") [argTypes, retType]) =
  Tfob (map s2type (s2list argTypes)) (s2type retType)
s2type se = error ("Type inconnu : " ++ showSexp se)

argToTuple :: Sexp -> (Var, Type)
argToTuple (Snode (Ssym v) [t]) = (v, s2type t) -- Argument avec son type
argToTuple se = error ("Argument invalide dans fix : " ++ showSexp se)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
-- La majorité du code provient de la correction du TP1 disponible
-- sur Studium. Elle a été adaptée pour gérer les types tau
s2l :: Sexp -> Lexp
s2l (Ssym "true") = Lbool True
s2l (Ssym "false") = Lbool False
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Snode (Ssym "if") [e1, e2, e3])
  = Ltest (s2l e1) (s2l e2) (s2l e3)
s2l (Snode (Ssym "fob") [args, body])
  = Lfob (map argToTuple (s2list args)) (s2l body)
s2l (Snode (Ssym "let") [x, e1, e2])
  = Llet (svar2lvar x) (s2l e1) (s2l e2)
s2l (Snode (Ssym "fix") [decls, body])
  = let sdecl2ldecl :: Sexp -> (Var, Lexp)
        -- Cas 1 : Déclaration simple (x e)
        sdecl2ldecl (Snode (Ssym v) [e]) = (v, s2l e)
        -- Cas 2 : Déclaration de fonction ((x args) e)
        sdecl2ldecl (Snode (Snode (Ssym v) args) [e])
          = (v, Lfob (map argToTuple args) (s2l e))
        -- Cas 3 : Déclaration complète ((x args) t e)
        sdecl2ldecl (Snode (Snode (Ssym v) args) [t, body2])
          = (v, Lfob (map argToTuple args) (Ltype (s2l body2) (s2type t)))
        -- Gestion des erreurs
        sdecl2ldecl se = error ("Déclaration inconnue dans fix : " ++ showSexp se)
    in Lfix (map sdecl2ldecl (s2list decls)) (s2l body)
s2l (Snode f args)
  = Lsend (s2l f) (map s2l args)
s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv Int Lexp -- L'entier indique le nombre d'arguments.

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type Env = [(Var, Type, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
           intbin = Tfob [Tnum, Tnum] Tnum
           boolbin = Tfob [Tnum, Tnum] Tbool

       in [("+", intbin,  binop Vnum (+)),
           ("*", intbin,  binop Vnum (*)),
           ("/", intbin,  binop Vnum div),
           ("-", intbin,  binop Vnum (-)),
           ("<", boolbin, binop Vbool (<)),
           (">", boolbin, binop Vbool (>)),
           ("≤", boolbin, binop Vbool (<=)),
           ("≥", boolbin, binop Vbool (>=)),
           ("=", boolbin, binop Vbool (==)),
           ("true",  Tbool, Vbool True),
           ("false", Tbool, Vbool False)]

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = [(Var, Type)]

-- `check c Γ e` renvoie le type de `e` dans l'environnement `Γ`.
-- Si `c` est vrai, on fait une vérification complète, alors que s'il
-- est faux, alors on présume que le code est typé correctement et on
-- se contente de chercher le type.
check :: Bool -> TEnv -> Lexp -> Type
check _ _ (Lnum _) = Tnum
check _ _ (Lbool _) = Tbool
check _ env (Lvar x) =
    case lookup x env of
        Just t -> t
        Nothing -> Terror ("Variable inconnue: " ++ x)
check True env (Ltype e τ) =
    let t = check True env e
    in if t == τ then τ else Terror ("Type mismatch: attendu " ++ show τ ++ ", obtenu " ++ show t)
check False _ (Ltype _ τ) = τ -- On présume que le type est correct avec `False`.
check True env (Ltest e1 e2 e3) =
    case check True env e1 of
        Tbool ->
            let t2 = check True env e2
                t3 = check True env e3
            in if t2 == t3 then t2 else Terror "Branches de if de types differents"
        _ -> Terror "Condition de if n'est pas un booléen"
check True env (Lsend f args) =
    case check True env f of
        Tfob argTypes returnType ->
            if length args == length argTypes
            then
                let argChecks = zipWith (\arg expectedType -> check True env arg == expectedType) args argTypes
                in if all id argChecks
                    then returnType
                    else Terror ("Types des arguments incorrects dans Lsend: " ++ show args ++ " attendu: " ++ show argTypes)
            else Terror ("Nombre d'arguments incorrect pour Lsend : attendu " ++ show (length argTypes) ++ ", obtenu " ++ show (length args))
        t -> Terror ("Expression appelée n'est pas une fonction, type obtenu : " ++ show t)
check True env (Lfob args body) =
    let argEnv = [(x, t) | (x, t) <- args] -- Arguments enrichis
        fullEnv = argEnv ++ env -- Environnement complet
    in case check True fullEnv body of
        Terror msg -> Terror ("Erreur dans le corps de Lfob : " ++ msg)
        t -> Tfob (map snd args) t -- Retourne le type de la fonction




---------------------------------------------------------------------------
-- Pré-évaluation
---------------------------------------------------------------------------

-- Dexp simplifie le code en éliminant deux aspects qui ne sont plus
-- utiles lors de l'évaluation:
-- - Les annotations de types.
-- - Les noms de variables, remplacés par des entiers qui représentent
--   la position de la variable dans l'environnement.  On appelle ces entiers
--   des [indexes de De Bruijn](https://en.wikipedia.org/wiki/De_Bruijn_index).

type VarIndex = Int

data Dexp = Dnum Int             -- Constante entière.
          | Dbool Bool           -- Constante Booléenne.
          | Dvar VarIndex        -- Référence à une variable.
          | Dtest Dexp Dexp Dexp -- Expression conditionelle.
          | Dfob Int Dexp        -- Construction de fobjet de N arguments.
          | Dsend Dexp [Dexp]    -- Appel de fobjet.
          | Dlet Dexp Dexp       -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Renvoie le "De Buijn index" de la variable, i.e. sa position
-- dans le contexte.
lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _) : xs) x2 n = if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI _ x _ = error ("Variable inconnue: " ++ show x)

-- Conversion de Lexp en Dexp.
-- Les types contenus dans le "TEnv" ne sont en fait pas utilisés.
l2d :: TEnv -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
l2d _ (Lbool b) = Dbool b
l2d tenv (Lvar v) = Dvar (lookupDI tenv v 0)
l2d tenv (Ltest e1 e2 e3) =
    Dtest (l2d tenv e1) (l2d tenv e2) (l2d tenv e3)
-- Remove la variable du `let` et on met dans env
-- pour l'eval de De Buijn futur
l2d tenv (Llet v e1 e2) =
    let de = l2d tenv e1
        newEnv = (v, Tnum) : tenv
        de' = l2d newEnv e2
    in Dlet de de'
-- La fonction check va regarder si les types sont bons
-- donc on peut les ignorer ici
l2d tenv (Lfob args body) =
    let nArgs = length args
        newEnv = [(v, t) | (v, t) <- args] ++ tenv
    in Dfob nArgs (l2d newEnv body)
l2d tenv (Lsend func args) =
    let func' = l2d tenv func
        args' = map (l2d tenv) args
    in Dsend func' args'
l2d tenv (Lfix decls body) = 
    let newEnv = [(v, Tnum) | (v, _) <- decls] ++ tenv
        decls' = map (\(_, e) -> l2d newEnv e) decls
    in Dfix decls' (l2d newEnv body)


---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
eval _   (Dnum n) = Vnum n
eval _   (Dbool b) = Vbool b
-- ¡¡COMPLÉTER ICI!!


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_x,_t,v) -> v) env0

-- Évaluation sans vérification de types.
evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se = let le = s2l se
               in case check True tenv0 le of
                    Terror err -> Right err
                    t -> Left (t, eval venv0 (l2d tenv0 le))

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
            in map tevalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf



-- ################ TEST ################
test1 :: Lexp
test1 = s2l (sexpOf "(fix (((f (x Num)) 42)) body)")

test2 :: Lexp
test2 = s2l (sexpOf "(fix (((f (x Num)) Bool (if x true false))) body)")

test3 :: Lexp
test3 = s2l (sexpOf "(let x 42 (if x true false))")