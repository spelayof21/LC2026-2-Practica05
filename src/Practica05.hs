module Practica05 where

import Terminos

-- Aplicar una sustitución a un término
apsubT :: Term -> Subst -> Term
apsubT (Var x) s =
  case lookup x s of
    Just t  -> t
    Nothing -> Var x
apsubT (Fun f args) s = Fun f (aplicarLista args s)

-- Función auxiliar para aplicar la sustitución a una lista de términos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista ts s = map (`apsubT` s) ts

-- Función que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus = filter (\(x, t) -> t /= Var x)

--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 =
  let s1Aplicada = map (\(x, t) -> (x, apsubT t s2)) s1
      dominioS1  = map fst s1
      s2Nueva    = filter (\(x, _) -> x `notElem` dominioS1) s2
  in simpSus (s1Aplicada ++ s2Nueva)

-- Verifica si una variable x ocurre en un término
ocurre :: Nombre -> Term -> Bool
ocurre x (Var y)     = x == y
ocurre x (Fun _ args) = any (ocurre x) args

-- Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
-- Caso: dos variables iguales
unifica (Var x) (Var y)
  | x == y    = [[]]
  | otherwise = [[( x, Var y)]]

-- Caso: variable con término
unifica (Var x) t
  | ocurre x t = []
  | otherwise  = [[(x, t)]]

-- Caso: término con variable
unifica t (Var x)
  | ocurre x t = []
  | otherwise  = [[(x, t)]]

-- Caso: dos constantes
unifica (Fun f []) (Fun g [])
  | f == g    = [[]]
  | otherwise = []

-- Caso: dos términos funcionales
unifica (Fun f args1) (Fun g args2)
  | f /= g                    = []
  | length args1 /= length args2 = []
  | otherwise                 = unificaListas args1 args2

-- Funcion que devuelve un unificador de dos terminos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]
unificaListas (t1:ts1) (t2:ts2) =
  case unifica t1 t2 of
    []    -> []
    [mu1] ->
      let ts1' = aplicarLista ts1 mu1
          ts2' = aplicarLista ts2 mu1
      in case unificaListas ts1' ts2' of
           []    -> []
           [mu2] -> [compSus mu1 mu2]
    _     -> []
unificaListas _ _ = []

-- Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj []  = [[]]
unificaConj [_] = [[]]
unificaConj (t1:t2:ts) =
  case unifica t1 t2 of
    []    -> []
    [mu1] ->
      let ts' = aplicarLista (t2:ts) mu1
      in case unificaConj ts' of
           []    -> []
           [mu2] -> [compSus mu1 mu2]
    _     -> []
