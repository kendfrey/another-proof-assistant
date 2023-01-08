module Reduce (reduce, reduceApp) where

import Syntax

reduce :: Env -> Term -> Value
reduce e (TVar _ n) = e !! n
reduce _ (THole n) = VStuck (SMVar n)
reduce _ TLevel = VLevel
reduce e (TLSucc u) = VLSucc (reduce e u)
reduce e (TLMax u v) = VLMax (reduce e u) (reduce e v)
reduce e (TType u) = VType (reduce e u)
reduce _ (TTypeOmega n) = VTypeOmega n
reduce e (TPi s a b) = VPi (reduce e a) (s, b, e)
reduce e (TLam s x) = VLam (s, x, e)
reduce e (TApp f x) = reduceApp (reduce e f) (reduce e x)
reduce e (TEq u a x y) = VEq (reduce e u) (reduce e a) (reduce e x) (reduce e y)
reduce e (TRefl u a x) = VRefl (reduce e u) (reduce e a) (reduce e x)
reduce e (TEqElim u v a x p ih y h) = reduceEqElim (reduce e u) (reduce e v) (reduce e a) (reduce e x) (reduce e p) (reduce e ih) (reduce e y) (reduce e h)

reduceApp :: Value -> Value -> Value
reduceApp (VLam (_, f, e)) x = reduce (x : e) f
reduceApp (VStuck f) x = VStuck (SApp f x)
reduceApp _ _ = error "reduceApp"

reduceEqElim :: Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value
reduceEqElim _ _ _ _ _ ih _ (VRefl _ _ _) = ih
reduceEqElim u v a x p ih y (VStuck h) = VStuck (SEqElim u v a x p ih y h)
reduceEqElim _ _ _ _ _ _ _ _ = error "reduceEqElim"