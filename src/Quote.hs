module Quote (quote, showValue, showStuck) where

import Reduce
import Syntax
import VarNames

quote :: Value -> Expr
quote (VStuck s) = quoteStuck s
quote VLevel = Var vLevel
quote (VLSucc u) = App (Var vLSucc) (quote u)
quote (VLMax u v) = App (App (Var vLMax) (quote u)) (quote v)
quote (VType u) = App (Var vType) (quote u)
quote (VTypeOmega n) = TypeOmega n
quote (VPi a b@(s, _, _)) = Pi s (quote a) (quoteClosure b)
quote (VLam x@(s, _, _)) = Lam s (quoteClosure x)
quote (VSigma u v a b) = App (App (App (App (Var vSigma) (quote u)) (quote v)) (quote a)) (quote b)
quote (VPair u v a b x y) = App (App (App (App (App (App (Var vPair) (quote u)) (quote v)) (quote a)) (quote b)) (quote x)) (quote y)
quote (VEq u a x y) = App (App (App (App (Var vEq) (quote u)) (quote a)) (quote x)) (quote y)
quote (VRefl u a x) = App (App (App (Var vRefl) (quote u)) (quote a)) (quote x)
quote (VEmpty u) = App (Var vEmpty) (quote u)
quote (VUnit u) = App (Var vUnit) (quote u)
quote (VStar u) = App (Var vStar) (quote u)
quote (VBool u) = App (Var vBool) (quote u)
quote (VTrue u) = App (Var vTrue) (quote u)
quote (VFalse u) = App (Var vFalse) (quote u)
quote (VW u v a b) = App (App (App (App (Var vW) (quote u)) (quote v)) (quote a)) (quote b)
quote (VSup u v a b i f) = App (App (App (App (App (App (Var vSup) (quote u)) (quote v)) (quote a)) (quote b)) (quote i)) (quote f)

quoteStuck :: Stuck -> Expr
quoteStuck (SVar s _) = Var s
quoteStuck (SMVar _) = Hole
quoteStuck (SApp f x) = App (quoteStuck f) (quote x)
quoteStuck (SSigmaElim u v w a b p ih x) = App (App (App (App (App (App (App (App (Var vSigmaElim) (quote u)) (quote v)) (quote w)) (quote a)) (quote b)) (quote p)) (quote ih)) (quoteStuck x)
quoteStuck (SEqElim u v a x p ih y h) = App (App (App (App (App (App (App (App (Var vEqElim) (quote u)) (quote v)) (quote a)) (quote x)) (quote p)) (quote ih)) (quote y)) (quoteStuck h)
quoteStuck (SEmptyElim u v p x) = App (App (App (App (Var vEmptyElim) (quote u)) (quote v)) (quote p)) (quoteStuck x)
quoteStuck (SBoolElim u v p ht hf x) = App (App (App (App (App (App (Var vBoolElim) (quote u)) (quote v)) (quote p)) (quote ht)) (quote hf)) (quoteStuck x)
quoteStuck (SWElim u v w a b p ih x) = App (App (App (App (App (App (App (App (Var vWElim) (quote u)) (quote v)) (quote w)) (quote a)) (quote b)) (quote p)) (quote ih)) (quoteStuck x)

quoteClosure :: Closure -> Expr
quoteClosure (s, x, e) = quote (reduce (VStuck (SVar s (error "quoteClosure")) : e) x)

showValue :: Value -> String
showValue = show . quote

showStuck :: Stuck -> String
showStuck = show . quoteStuck