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
reduce e (TSigma u v a b) = VSigma (reduce e u) (reduce e v) (reduce e a) (reduce e b)
reduce e (TPair u v a b x y) = VPair (reduce e u) (reduce e v) (reduce e a) (reduce e b) (reduce e x) (reduce e y)
reduce e (TSigmaElim u v w a b p ih x) = reduceSigmaElim (reduce e u) (reduce e v) (reduce e w) (reduce e a) (reduce e b) (reduce e p) (reduce e ih) (reduce e x)
reduce e (TEq u a x y) = VEq (reduce e u) (reduce e a) (reduce e x) (reduce e y)
reduce e (TRefl u a x) = VRefl (reduce e u) (reduce e a) (reduce e x)
reduce e (TEqElim u v a x p ih y h) = reduceEqElim (reduce e u) (reduce e v) (reduce e a) (reduce e x) (reduce e p) (reduce e ih) (reduce e y) (reduce e h)
reduce e (TEmpty u) = VEmpty (reduce e u)
reduce e (TEmptyElim u v p x) = reduceEmptyElim (reduce e u) (reduce e v) (reduce e p) (reduce e x)
reduce e (TUnit u) = VUnit (reduce e u)
reduce e (TStar u) = VStar (reduce e u)
reduce e (TBool u) = VBool (reduce e u)
reduce e (TTrue u) = VTrue (reduce e u)
reduce e (TFalse u) = VFalse (reduce e u)
reduce e (TBoolElim u v p ht hf x) = reduceBoolElim (reduce e u) (reduce e v) (reduce e p) (reduce e ht) (reduce e hf) (reduce e x)

reduceApp :: Value -> Value -> Value
reduceApp (VLam (_, f, e)) x = reduce (x : e) f
reduceApp (VStuck f) x = VStuck (SApp f x)
reduceApp _ _ = error "reduceApp"

reduceSigmaElim :: Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value
reduceSigmaElim _ _ _ _ _ _ ih (VPair _ _ _ _ x y) = reduceApp (reduceApp ih x) y
reduceSigmaElim u v w a b p ih (VStuck x) = VStuck (SSigmaElim u v w a b p ih x)
reduceSigmaElim _ _ _ _ _ _ _ _ = error "reduceSigmaElim"

reduceEqElim :: Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value
reduceEqElim _ _ _ _ _ ih _ (VRefl _ _ _) = ih
reduceEqElim u v a x p ih y (VStuck h) = VStuck (SEqElim u v a x p ih y h)
reduceEqElim _ _ _ _ _ _ _ _ = error "reduceEqElim"

reduceEmptyElim :: Value -> Value -> Value -> Value -> Value
reduceEmptyElim u v p (VStuck x) = VStuck (SEmptyElim u v p x)
reduceEmptyElim _ _ _ _ = error "reduceEmptyElim"

reduceBoolElim :: Value -> Value -> Value -> Value -> Value -> Value -> Value
reduceBoolElim _ _ _ ht _ (VTrue _) = ht
reduceBoolElim _ _ _ _ hf (VFalse _) = hf
reduceBoolElim u v p ht hf (VStuck x) = VStuck (SBoolElim u v p ht hf x)
reduceBoolElim _ _ _ _ _ _ = error "reduceBoolElim"