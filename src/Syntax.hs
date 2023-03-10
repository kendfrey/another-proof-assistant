module Syntax (
  Expr(..),
  Term(..),
  Env,
  Closure,
  Value(..),
  Stuck(..),
  lam,
  pi
  ) where

import Prelude hiding (pi)

data Expr
  = Var String
  | Hole String
  | TypeOmega Int
  | Pi String Expr Expr
  | Lam String Expr
  | App Expr Expr

pi :: Expr -> Expr -> Expr
pi = Pi "_"

lam :: Expr -> Expr
lam = Lam "_"

data Precedence
  = PZero
  | PApp
  | PPi
  | PLam
  deriving (Eq, Ord, Bounded, Enum)

instance Show Expr where
  show = showExpr maxBound
    where
    showExpr :: Precedence -> Expr -> String
    showExpr _ (Var s) = s
    showExpr _ (Hole s) = "?" ++ s
    showExpr _ (TypeOmega 0) = "Typew"
    showExpr _ (TypeOmega n) = "Typew+" ++ show n
    showExpr p (Pi "_" a b) = parens p PPi $ showExpr (pred PPi) a ++ " -> " ++ showExpr PPi b
    showExpr p (Pi s a b) = parens p PPi $ "(" ++ s ++ " : " ++ showExpr maxBound a ++ ") -> " ++ showExpr PPi b
    showExpr p (Lam s x) = parens p PLam $ s ++ " => " ++ showExpr PLam x
    showExpr p (App f x) = parens p PApp $ showExpr PApp f ++ " " ++ showExpr (pred PApp) x
    parens :: Precedence -> Precedence -> String -> String
    parens p pmin s | p >= pmin = s
                    | otherwise = "(" ++ s ++ ")"

data Term
  = TVar String Int
  | THole String Int
  | TLevel
  | TLSucc Term
  | TLMax Term Term
  | TType Term
  | TTypeOmega Int
  | TPi String Term Term
  | TLam String Term
  | TApp Term Term
  | TSigma Term Term Term Term
  | TPair Term Term Term Term Term Term
  | TSigmaElim Term Term Term Term Term Term Term Term
  | TEq Term Term Term Term
  | TRefl Term Term Term
  | TEqElim Term Term Term Term Term Term Term Term
  | TEmpty Term
  | TEmptyElim Term Term Term Term
  | TUnit Term
  | TStar Term
  | TBool Term
  | TTrue Term
  | TFalse Term
  | TBoolElim Term Term Term Term Term Term
  | TW Term Term Term Term
  | TSup Term Term Term Term Term Term
  | TWElim Term Term Term Term Term Term Term Term
  | TQuot Term Term Term Term
  | TPack Term Term Term Term Term
  | TQuotElim Term Term Term Term Term Term Term Term Term
  deriving (Show)

type Env = [Value]

type Closure = (String, Term, Env)

data Value
  = VStuck Stuck (Maybe Value)
  | VLevel
  | VLSucc Value
  | VLMax Value Value
  | VType Value
  | VTypeOmega Int
  | VPi Value Closure
  | VLam Closure
  | VSigma Value Value Value Value
  | VPair Value Value Value Value Value Value
  | VEq Value Value Value Value
  | VRefl Value Value Value
  | VEmpty Value
  | VUnit Value
  | VStar Value
  | VBool Value
  | VTrue Value
  | VFalse Value
  | VW Value Value Value Value
  | VSup Value Value Value Value Value Value
  | VQuot Value Value Value Value
  | VPack Value Value Value Value Value

data Stuck
  = SVar String Int
  | SMVar String Int
  | SApp Stuck Value
  | SSigmaElim Value Value Value Value Value Value Value Stuck
  | SEqElim Value Value Value Value Value Value Value Stuck
  | SEmptyElim Value Value Value Stuck
  | SBoolElim Value Value Value Value Value Stuck
  | SWElim Value Value Value Value Value Value Value Stuck
  | SQuotElim Value Value Value Value Value Value Value Value Stuck