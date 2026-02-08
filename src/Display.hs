module Display where

import Syntax
import Lexer

showTm' :: TermNode -> String
showTm' t = showTm [] t

showTm :: Context -> TermNode -> String
showTm ctx t = let tm = getTm t in
  case tm of
    TmVar k l -> let ctxLength = length ctx in
      if (l == ctxLength)
        then getNameFromContext ctx k
        else error $ tmVarErr l ctxLength
    TmAbs x ty t1 ->
      let x' = fixName ctx x
       in "(" ++ "λ" ++ x' ++ ":" ++ showType ty ++ "." ++ showTm ((x', ty):ctx) t1 ++ ")"
    TmApp t1 t2 -> "(" ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
    TmSucc t1 -> "(" ++ "succ " ++ showTm' t1 ++ ")"
    TmPred t1 -> "(" ++ "pred " ++ showTm' t1 ++ ")"
    TmIsZero t1 -> "(" ++ "iszero " ++ showTm' t1 ++ ")"
    TmZero -> "0"
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ showTm' t1
      ++ " then " ++ showTm' t2
      ++ " else " ++ showTm' t3 ++ ")"
    TmUnit -> "unit"
    TmAscribe t1 ty -> "(" ++ showTm' t1 ++ " as " ++ showType ty ++ ")"
    TmSeq t1 t2 -> "(" ++ showTm' t1 ++ ";" ++ showTm' t2 ++ ")"
    TmWildCard ty t2 -> "(" ++ "λ_:" ++ showType ty ++ "." ++ showTm' t2 ++ ")"
  where showTm' = showTm ctx
        tmVarErr l ctxLength = "TmVar: bad context length: " ++ show l ++ "/=" ++ (show $ ctxLength)

getNameFromContext :: Context -> Index -> Name
getNameFromContext ctx ind | ind < length ctx = fst (ctx !! ind)
                           | otherwise = error "TmVar: no name context for var"

fixName :: Context -> Name -> Name
fixName ctx x | (length $ filter ((==) x . fst) ctx) < 1 = x
              | otherwise = fixName ctx (x ++ "\'")

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "Absolute Offset" ++ show p ++ "\n"
  ++ "Line" ++ show l ++ "\n"
  ++ "Column" ++ show c ++ "\n"

showType :: Type -> String
showType ty =
  case ty of
    TyNat -> "Nat"
    TyBool -> "Bool"
    TyUnit -> "Unit"
    TyArr ty1 ty2 -> "(" ++ showType ty1 ++ "->" ++ showType ty2 ++ ")"
