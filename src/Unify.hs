module Unify (unify) where

import Context
import Data.Map (Map, singleton, unionWith)
import qualified Data.Map as M
import Error
import Quote
import Reduce
import Syntax

unify :: MonadTrace m => Ctx -> Value -> Value -> m ()
unify _c _x _y = trace ("\nUnifying " ++ showValue _x ++ " and " ++ showValue _y) $ unify' _c _x _y
  where
  unify' :: MonadTrace m => Ctx -> Value -> Value -> m ()
  unify' c (VStuck x) (VStuck y) = unifyStuck c x y
  unify' _ VLevel VLevel = return ()
  unify' _ x@(VLSucc _) y = unifyLevel x y
  unify' _ x y@(VLSucc _) = unifyLevel x y
  unify' _ x@(VLMax _ _) y = unifyLevel x y
  unify' _ x y@(VLMax _ _) = unifyLevel x y
  unify' c (VType x) (VType y) = unify c x y
  unify' _ (VTypeOmega x) (VTypeOmega y) | x == y = return ()
  unify' c (VPi a (s, b, e)) (VPi a' (s', b', e')) = do
    unify c a a'
    unify c (reduce (newVar c s : e) b) (reduce (newVar c s' : e') b')
  unify' c x@(VLam _) y = unifyFun c x y
  unify' c x y@(VLam _) = unifyFun c x y
  unify' c (VEq u a x y) (VEq u' a' x' y') = do
    unify c u u'
    unify c a a'
    unify c x x'
    unify c y y'
  unify' _ (VRefl _ _ _) _ = return ()
  unify' _ _ (VRefl _ _ _) = return ()
  unify' _ _ _ = fail "Could not unify"

unifyStuck :: MonadTrace m => Ctx -> Stuck -> Stuck -> m ()
unifyStuck _c _x _y = trace ("\nUnifying " ++ showStuck _x ++ " and " ++ showStuck _y) $ unifyStuck' _c _x _y
  where
  unifyStuck' :: MonadTrace m => Ctx -> Stuck -> Stuck -> m ()
  unifyStuck' _ (SVar _ x) (SVar _ y) | x == y = return ()
  unifyStuck' _ (SMVar x) (SMVar y) | x == y = return ()
  unifyStuck' c (SApp f x) (SApp g y) = do
    unifyStuck c f g
    unify c x y
  unifyStuck' c (SEqElim u v a x p ih y _) (SEqElim u' v' a' x' p' ih' y' _) = do
    unify c u u'
    unify c v v'
    unify c a a'
    unify c x x'
    unify c p p'
    unify c ih ih'
    unify c y y'
  unifyStuck' _ _ _ = fail "Could not unify"

unifyLevel :: MonadFail m => Value -> Value -> m ()
unifyLevel x y = do
  x' <- reduceLevel x
  y' <- reduceLevel y
  if x' == y' then
    return ()
  else
    fail "Could not unify"

reduceLevel :: MonadFail m => Value -> m (Map Int Int)
reduceLevel (VStuck (SVar _ x)) = return $ singleton x 0
reduceLevel (VLSucc x) = M.map (+1) <$> reduceLevel x
reduceLevel (VLMax x y) = unionWith max <$> reduceLevel x <*> reduceLevel y
reduceLevel x = fail $ "Unsupported universe level " ++ showValue x

unifyFun :: MonadTrace m => Ctx -> Value -> Value -> m ()
unifyFun c f g = let x = newVar c "#" in unify (c |- Def True "#" (error "unifyFun") x) (reduceApp f x) (reduceApp g x)