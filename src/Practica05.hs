module Practica05 where

import Terminos

--Aplicar una sustitucion a un termino
buscar :: Nombre -> Subst -> Term
buscar n [] = Var n
buscar n ((x,t):xs)
    | n == x = t
    | otherwise = buscar n xs

apsubT :: Term -> Subst -> Term
apsubT (Var x) s = buscar x s
apsubT (Fun f ts) s = Fun f (aplicarLista ts s)


--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista [] s = []
aplicarLista (x:xs) s =
    apsubT x s : aplicarLista xs s


--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus [] = []

simpSus ((x,t):xs)
    | t == Var x = simpSus xs
    | otherwise = (x,t) : simpSus xs

--Funcion que calcula la composicion de dos sustituciones

pertenece :: Nombre -> Subst -> Bool
pertenece x [] = False

pertenece x ((y,t):ys)
    | x == y = True
    | otherwise = pertenece x ys

apSubst :: Subst -> Subst -> Subst
apSubst [] s = []

apSubst ((x,t):xs) s =
    (x, apsubT t s) : apSubst xs s

agregaNuevas :: Subst -> Subst -> Subst
agregaNuevas s1 [] = []

agregaNuevas s1 ((x,t):xs)
    | pertenece x s1 = agregaNuevas s1 xs
    | otherwise = (x,t) : agregaNuevas s1 xs

compSus :: Subst -> Subst -> Subst
compSus s1 s2 =
    simpSus (apSubst s1 s2 ++ agregaNuevas s1 s2)

-- Occurs check

ocurre :: Nombre -> Term -> Bool
ocurre x (Var y) = x == y

ocurre x (Fun f ts) = ocurreLista x ts

ocurreLista :: Nombre -> [Term] -> Bool
ocurreLista x [] = False

ocurreLista x (t:ts)
    | ocurre x t = True
    | otherwise = ocurreLista x ts

--Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]

-- variables iguales
unifica (Var x) (Var y)
    | x == y = [[]]

-- variable contra término
unifica (Var x) t
    | ocurre x t = []
    | otherwise = [[(x,t)]]

-- término contra variable
unifica t (Var x)
    | ocurre x t = []
    | otherwise = [[(x,t)]]

-- funciones
unifica (Fun f ts) (Fun g rs)
    | f /= g = []
    | longitud ts /= longitud rs = []
    | otherwise = unificaListas ts rs


longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]

unificaListas [] [] = [[]]

unificaListas (x:xs) (y:ys) =
    aux (unifica x y) xs ys

unificaListas _ _ = []


aux :: [Subst] -> [Term] -> [Term] -> [Subst]

aux [] xs ys = []

aux (s:ss) xs ys =
    aux2 s (unificaListas (aplicarLista xs s)
                             (aplicarLista ys s))


aux2 :: Subst -> [Subst] -> [Subst]

aux2 s [] = []

aux2 s (r:rs) =
    [compSus s r]

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj [] = [[]]
unificaConj [x] = [[]]
unificaConj (x:xs) =
    auxConj x xs []

auxConj :: Term -> [Term] -> Subst -> [Subst]
auxConj _ [] s = [s]
auxConj x (y:xs) s =
    auxConj2 (apsubT x s) xs s (unifica (apsubT x s) (apsubT y s))

auxConj2 :: Term -> [Term] -> Subst -> [Subst] -> [Subst]
auxConj2 _ _ _ [] = []
auxConj2 x' xs s [u] = auxConj x' xs (compSus s u)
auxConj2 _ _ _ _ = []