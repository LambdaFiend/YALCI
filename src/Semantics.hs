module Semantics where

import Syntax

-- considering that we're using De Bruijn's representation:

-- shift c d:
-- of a var k = if k < c then var k else var (k + d)
-- of an abs t1 = abs (shift (c + 1) d of t1)
-- of an app t1 t2 = app (shift c d of t1) (shift c d of t2)

-- substitution of bound var j by term s:
-- on a var k = if k == j then s else k
-- on an abs t1 = abs (substitution of bound var (j + 1) by term (shift 1 0 s) on t1)
-- on an app t1 t2 = app (substitution of bound var j by s on t1) (substitution of bound var j by s on t2)


shift :: Index -> Index -> Term -> Term
shift c d t =
  case t of
    TmVar k l -> TmVar (if k < c then k else k + d) (l + d)
    TmAbs x ty t1 -> TmAbs x ty (shift (c + 1) d t1)
    TmApp t1 t2 -> TmApp (shift' t1) (shift' t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (shift' t1) (shift' t2) (shift' t3)
    TmZero -> TmZero
    TmSucc t1 -> TmSucc $ shift' t1
    TmPred t1 -> TmPred $ shift' t1
  where shift' = shift c d    

subst :: Index -> Term -> Term -> Term
subst j s t =
  case t of
    TmVar k l -> if k == j then s else TmVar k l
    TmAbs x ty t1 -> TmAbs x ty (subst (j + 1) (shift 1 0 s) t1)
    TmApp t1 t2 -> TmApp (subst j s t1) (subst j s t2)
    TmTrue  -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (subst j s t1) (subst j s t2) (subst j s t3)
    TmZero -> TmZero
    TmSucc t1 -> TmSucc $ subst' t1
    TmPred t1 -> TmPred $ subst' t1
  where subst' = subst j s

evalSubst :: Term -> Term -> Term
evalSubst s t = shift 0 (-1) (subst 0 (shift 0 1 s) t)

isVal :: Term -> Bool
isVal t =
  case t of
    TmAbs _ _ _ -> True
    TmTrue -> True
    TmFalse -> True
    TmZero -> True
    TmSucc nv | isVal nv -> True
    _ -> False

eval1 :: Term -> Term
eval1 t =
  case t of
    TmApp (TmAbs _ _ t12) v2 | isVal v2 -> evalSubst v2 t12
    TmApp v1 t2 | isVal v1 ->
      let t2' = eval1 t2
       in TmApp v1 t2'
    TmApp t1 t2 ->
      let t1' = eval1 t1
       in TmApp t1' t2
    TmIf TmTrue t2 t3 -> t2
    TmIf TmFalse t2 t3 -> t3
    TmIf t1 t2 t3 ->
      let t1' = eval1 t1
       in TmIf t1' t2 t3
    TmSucc t1 | not $ isVal t1 -> TmSucc $ eval1 t1
    TmPred TmZero -> TmZero
    TmPred (TmSucc nv1) | isVal nv1 -> nv1
    TmPred t1 -> TmPred $ eval1 t1
    TmIsZero TmZero -> TmTrue
    TmIsZero (TmSucc nv1) | isVal nv1 -> TmFalse
    TmIsZero t1 -> TmIsZero $ eval1 t1
    _ -> error "No Rule Applies"

eval :: Term -> Term
eval t | isVal t   = t
       | otherwise = eval $ eval1 t
