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

quoteStuck :: Stuck -> Expr
quoteStuck (SVar s _) = Var s
quoteStuck (SMVar _) = Hole
quoteStuck (SApp f x) = App (quoteStuck f) (quote x)

quoteClosure :: Closure -> Expr
quoteClosure (s, x, e) = quote (reduce (VStuck (SVar s (error "quoteClosure")) : e) x)

showValue :: Value -> String
showValue = show . quote

showStuck :: Stuck -> String
showStuck = show . quoteStuck