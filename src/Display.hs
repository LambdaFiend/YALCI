module Display where

import Syntax

printTm :: Context -> Term -> String
printTm ctx t =
  case t of
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ printTm ctx t1
      ++ " then " ++ printTm ctx t2
      ++ " else " ++ printTm ctx t3 ++ ")"
    TmVar k l ->
      if (l == length ctx)
        then getNameFromContext ctx k
        else error "TmVar: bad context length"
    TmAbs x ty t1 ->
      let x' = fixName ctx x
       in "(" ++ "λ" ++ x' ++ "." ++ printTm ((x', ty):ctx) t1 ++ ")"
    TmApp t1 t2 -> "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"

getNameFromContext :: Context -> Index -> Name
getNameFromContext ctx ind | ind < length ctx = fst (ctx !! ind)
                           | otherwise = error "TmVar: no name context for var"

fixName :: Context -> Name -> Name
fixName ctx x | (length $ filter ((==) x . fst) ctx) < 1 = x
              | otherwise = fixName ctx (x ++ "\'")
