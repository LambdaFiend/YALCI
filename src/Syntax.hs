module Syntax where


type Name = String
type Index = Int

data Term
  = TmVar Index Index
  | TmAbs Name Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

data Type
  = TyBool
  | TyNat
  | TyArr Type Type
  deriving (Eq, Show)

type Context = [(Name, Type)]
