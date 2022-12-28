module Elaborate (Goal, elaborate, elaborateType) where

import Context
import Control.Monad.Trans.Accum
import Error
import Quote
import Reduce
import Syntax
import Unify

type Goal = (Int, Ctx, Value)

elaborate :: MonadTrace m => Ctx -> Value -> Expr -> AccumT [Goal] m Term
elaborate _c _a _x = trace ("\nElaborating " ++ show _x ++ " as " ++ showValue _a) $ elaborate' _c _a _x
  where
  elaborate' :: MonadTrace m => Ctx -> Value -> Expr -> AccumT [Goal] m Term
  elaborate' c a (Var s) = do
    (d, n) <- getVar s c
    unify c a (defType d)
    return $ TVar s n
  elaborate' c a Hole = do
    n <- looks length
    add [(n, c, a)]
    return $ THole n
  elaborate' c a x@(App _ _) = do
    (x', a') <- infer c x
    unify c a a'
    return x'
  elaborate' c a@(VType _) x = do
    (x', a') <- elaborateType c x
    unify c a a'
    return x'
  elaborate' c a@(VTypeOmega _) x = do
    (x', a') <- elaborateType c x
    unify c a a'
    return x'
  elaborate' c (VPi a (_, b, e)) (Lam s f) = do
    let x = newVar c s
    f' <- elaborate (c |- Def True s a x) (reduce (x : e) b) f
    return $ TLam s f'
  elaborate' _ _ _ = fail "Could not elaborate"

elaborateType :: MonadTrace m => Ctx -> Expr -> AccumT [Goal] m (Term, Value)
elaborateType _c _x = trace ("\nElaborating " ++ show _x ++ " as a type") $ elaborateType' _c _x
  where
  elaborateType' :: MonadTrace m => Ctx -> Expr -> AccumT [Goal] m (Term, Value)
  elaborateType' c (Var s) = do
    (d, n) <- getVar s c
    checkType (defType d)
    return (TVar s n, defType d)
  elaborateType' _ (TypeOmega n) = return (TTypeOmega n, VTypeOmega (n + 1))
  elaborateType' c (Pi s a b) = do
    (a', ta) <- elaborateType c a
    (b', tb) <- elaborateType (c |- Def True s (reduce (env c) a') (newVar c s)) b
    return (TPi s a' b', tpMax ta tb)
  elaborateType' c x@(App _ _) = do
    (x', a) <- infer c x
    checkType a
    return (x', a)
  elaborateType' _ _ = fail "Could not elaborate"

infer :: MonadTrace m => Ctx -> Expr -> AccumT [Goal] m (Term, Value)
infer _c _x = trace ("\nInferring the type of " ++ show _x) $ infer' _c _x
  where
  infer' :: MonadTrace m => Ctx -> Expr -> AccumT [Goal] m (Term, Value)
  infer' c (Var s) = do
    (d, n) <- getVar s c
    return (TVar s n, defType d)
  infer' _ Hole = fail "Cannot infer the type of a hole"
  infer' _ (TypeOmega n) = return (TTypeOmega n, VTypeOmega (n + 1))
  infer' c (Pi s a b) = do
    (a', ta) <- elaborateType c a
    (b', tb) <- elaborateType (c |- Def True s (reduce (env c) a') (newVar c s)) b
    return (TPi s a' b', tpMax ta tb)
  infer' _ (Lam _ _) = fail "Cannot infer the type of a lambda"
  infer' c (App f x) = do
    (f', a') <- infer c f
    (a, (_, b, e)) <- getPi a'
    x' <- elaborate c a x
    return (TApp f' x', reduce (reduce (env c) x' : e) b)

tpMax :: Value -> Value -> Value
tpMax (VType x) (VType y) = VType (VLMax x y)
tpMax (VType _) (VTypeOmega n) = VTypeOmega n
tpMax (VTypeOmega n) (VType _) = VTypeOmega n
tpMax (VTypeOmega n) (VTypeOmega m) = VTypeOmega (max n m)
tpMax _ _ = error "tpMax"

getPi :: MonadFail m => Value -> m (Value, Closure)
getPi (VPi a b) = return (a, b)
getPi _ = fail "Function expected"

checkType :: MonadFail m => Value -> m ()
checkType (VType _) = return ()
checkType (VTypeOmega _) = return ()
checkType _ = fail "Type expected"