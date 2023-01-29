module Def (addDef, defaultCtx) where

import Context
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Elaborate
import Error
import Reduce
import Syntax
import VarNames

defaultCtx :: Ctx
defaultCtx = []
  |- Def False vLevel
    (VTypeOmega 0)
    VLevel
  |- Def False vLSucc
    (VPi VLevel ("_", TLevel, []))
    (VLam ("u", TLSucc (TVar "u" 0), []))
  |- Def False vLMax
    (VPi VLevel ("_", TPi "_" TLevel TLevel, []))
    (VLam ("u", TLam "v" (TLMax (TVar "u" 1) (TVar "v" 0)), []))
  |- Def False vType
    (VPi VLevel ("u", TType (TLSucc (TVar "u" 0)), []))
    (VLam ("u", TType (TVar "u" 0), []))
  |- Def False vSigma
    (VPi VLevel ("u", TPi "v" TLevel (TPi "A" (TType (TVar "u" 1)) (TPi "_" (TPi "_" (TVar "A" 0) (TType (TVar "v" 2))) (TType (TLMax (TVar "u" 3) (TVar "v" 2))))), []))
    (VLam ("u", TLam "v" (TLam "A" (TLam "B" (TSigma (TVar "u" 3) (TVar "v" 2) (TVar "A" 1) (TVar "B" 0)))), []))
  |- Def False vPair
    (VPi VLevel ("u", TPi "v" TLevel (TPi "A" (TType (TVar "u" 1)) (TPi "B" (TPi "_" (TVar "A" 0) (TType (TVar "v" 2))) (TPi "x" (TVar "A" 1) (TPi "_" (TApp (TVar "B" 1) (TVar "x" 0)) (TSigma (TVar "u" 5) (TVar "v" 4) (TVar "A" 3) (TVar "B" 2)))))), []))
    (VLam ("u", TLam "v" (TLam "A" (TLam "B" (TLam "x" (TLam "y" (TPair (TVar "u" 5) (TVar "v" 4) (TVar "A" 3) (TVar "B" 2) (TVar "x" 1) (TVar "y" 0)))))), []))
  |- Def False vSigmaElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "w" TLevel (TPi "A" (TType (TVar "u" 2)) (TPi "B" (TPi "_" (TVar "A" 0) (TType (TVar "v" 3))) (TPi "P" (TPi "_" (TSigma (TVar "u" 4) (TVar "v" 3) (TVar "A" 1) (TVar "B" 0)) (TType (TVar "w" 3))) (TPi "_" (TPi "x" (TVar "A" 2) (TPi "y" (TApp (TVar "B" 2) (TVar "x" 0)) (TApp (TVar "P" 2) (TPair (TVar "u" 7) (TVar "v" 6) (TVar "A" 4) (TVar "B" 3) (TVar "x" 1) (TVar "y" 0))))) (TPi "x" (TSigma (TVar "u" 6) (TVar "v" 5) (TVar "A" 3) (TVar "B" 2)) (TApp (TVar "P" 2) (TVar "x" 0)))))))), []))
    (VLam ("u", (TLam "v" (TLam "w" (TLam "A" (TLam "B" (TLam "P" (TLam "ih" (TLam "x" (TSigmaElim (TVar "u" 7) (TVar "v" 6) (TVar "w" 5) (TVar "A" 4) (TVar "B" 3) (TVar "P" 2) (TVar "ih" 1) (TVar "x" 0))))))))), []))
  |- Def False vEq
    (VPi VLevel ("u", TPi "A" (TType (TVar "u" 0)) (TPi "_" (TVar "A" 0) (TPi "_" (TVar "A" 1) (TType (TVar "u" 3)))), []))
    (VLam ("u", TLam "A" (TLam "x" (TLam "y" (TEq (TVar "u" 3) (TVar "A" 2) (TVar "x" 1) (TVar "y" 0)))), []))
  |- Def False vRefl
    (VPi VLevel ("u", TPi "A" (TType (TVar "u" 0)) (TPi "x" (TVar "A" 0) (TEq (TVar "u" 2) (TVar "A" 1) (TVar "x" 0) (TVar "x" 0))), []))
    (VLam ("u", TLam "A" (TLam "x" (TRefl (TVar "u" 2) (TVar "A" 1) (TVar "x" 0))), []))
  |- Def False vEqElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "A" (TType (TVar "u" 1)) (TPi "x" (TVar "A" 0) (TPi "P" (TPi "y" (TVar "A" 1) (TPi "_" (TEq (TVar "u" 4) (TVar "a" 2) (TVar "x" 1) (TVar "y" 0)) (TType (TVar "v" 4)))) (TPi "_" (TApp (TApp (TVar "P" 0) (TVar "x" 1)) (TRefl (TVar "u" 4) (TVar "A" 2) (TVar "x" 1))) (TPi "y" (TVar "A" 3) (TPi "h" (TEq (TVar "u" 6) (TVar "A" 4) (TVar "x" 3) (TVar "y" 0)) (TApp (TApp (TVar "P" 3) (TVar "y" 1)) (TVar "h" 0)))))))), []))
    (VLam ("u", TLam "v" (TLam "A" (TLam "x" (TLam "P" (TLam "ih" (TLam "y" (TLam "h" (TEqElim (TVar "u" 7) (TVar "v" 6) (TVar "A" 5) (TVar "x" 4) (TVar "P" 3) (TVar "ih" 2) (TVar "y" 1) (TVar "h" 0)))))))), []))
  |- Def False vEmpty
    (VPi VLevel ("u", TType (TVar "u" 0), []))
    (VLam ("u", TEmpty (TVar "u" 0), []))
  |- Def False vEmptyElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "P" (TPi "_" (TEmpty (TVar "u" 1)) (TType (TVar "v" 1))) (TPi "x" (TEmpty (TVar "u" 2)) (TApp (TVar "P" 1) (TVar "x" 0)))), []))
    (VLam ("u", TLam "v" (TLam "P" (TLam "x" (TEmptyElim (TVar "u" 3) (TVar "v" 2) (TVar "P" 1) (TVar "x" 0)))), []))
  |- Def False vUnit
    (VPi VLevel ("u", TType (TVar "u" 0), []))
    (VLam ("u", TUnit (TVar "u" 0), []))
  |- Def False vStar
    (VPi VLevel ("u", TUnit (TVar "u" 0), []))
    (VLam ("u", TStar (TVar "u" 0), []))
  |- Def False vUnitElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "P" (TPi "_" (TUnit (TVar "u" 1)) (TType (TVar "v" 1))) (TPi "_" (TApp (TVar "P" 0) (TStar (TVar "u" 2))) (TPi "x" (TUnit (TVar "u" 3)) (TApp (TVar "P" 2) (TVar "x" 0))))), []))
    (VLam ("u", TLam "v" (TLam "P" (TLam "ih" (TLam "x" (TVar "ih" 1)))), []))
  |- Def False vBool
    (VPi VLevel ("u", TType (TVar "u" 0), []))
    (VLam ("u", TBool (TVar "u" 0), []))
  |- Def False vTrue
    (VPi VLevel ("u", TBool (TVar "u" 0), []))
    (VLam ("u", TTrue (TVar "u" 0), []))
  |- Def False vFalse
    (VPi VLevel ("u", TBool (TVar "u" 0), []))
    (VLam ("u", TFalse (TVar "u" 0), []))
  |- Def False vBoolElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "P" (TPi "_" (TBool (TVar "u" 1)) (TType (TVar "v" 1))) (TPi "_" (TApp (TVar "P" 0) (TTrue (TVar "u" 2))) (TPi "_" (TApp (TVar "P" 1) (TFalse (TVar "u" 3))) (TPi "x" (TBool (TVar "u" 4)) (TApp (TVar "P" 3) (TVar "x" 0)))))), []))
    (VLam ("u", TLam "v" (TLam "P" (TLam "ht" (TLam "hf" (TLam "x" (TBoolElim (TVar "u" 5) (TVar "v" 4) (TVar "P" 3) (TVar "ht" 2) (TVar "hf" 1) (TVar "x" 0)))))), []))
  |- Def False vW
    (VPi VLevel ("u", TPi "v" TLevel (TPi "A" (TType (TVar "u" 1)) (TPi "_" (TPi "_" (TVar "A" 0) (TType (TVar "v" 2))) (TType (TLMax (TVar "u" 3) (TVar "v" 2))))), []))
    (VLam ("u", TLam "v" (TLam "A" (TLam "B" (TW (TVar "u" 3) (TVar "v" 2) (TVar "A" 1) (TVar "B" 0)))), []))
  |- Def False vSup
    (VPi VLevel ("u", TPi "v" TLevel (TPi "A" (TType (TVar "u" 1)) (TPi "B" (TPi "_" (TVar "A" 0) (TType (TVar "v" 2))) (TPi "i" (TVar "A" 1) (TPi "_" (TPi "_" (TApp (TVar "B" 1) (TVar "i" 0)) (TW (TVar "u" 5) (TVar "v" 4) (TVar "A" 3) (TVar "B" 2))) (TW (TVar "u" 5) (TVar "v" 4) (TVar "A" 3) (TVar "B" 2)))))), []))
    (VLam ("u", TLam "v" (TLam "A" (TLam "B" (TLam "i" (TLam "f" (TSup (TVar "u" 5) (TVar "v" 4) (TVar "A" 3) (TVar "B" 2) (TVar "i" 1) (TVar "f" 0)))))), []))
  |- Def False vWElim
    (VPi VLevel ("u", TPi "v" TLevel (TPi "w" TLevel (TPi "A" (TType (TVar "u" 2)) (TPi "B" (TPi "_" (TVar "A" 0) (TType (TVar "v" 3))) (TPi "P" (TPi "_" (TW (TVar "u" 4) (TVar "v" 3) (TVar "A" 1) (TVar "B" 0)) (TType (TVar "w" 3))) (TPi "_" (TPi "i" (TVar "A" 2) (TPi "f" (TPi "_" (TApp (TVar "B" 2) (TVar "i" 0)) (TW (TVar "u" 7) (TVar "v" 6) (TVar "A" 4) (TVar "B" 3))) (TPi "_" (TPi "j" (TApp (TVar "B" 3) (TVar "i" 1)) (TApp (TVar "P" 3) (TApp (TVar "f" 1) (TVar "j" 0)))) (TApp (TVar "P" 3) (TSup (TVar "u" 8) (TVar "v" 7) (TVar "A" 5) (TVar "B" 4) (TVar "i" 2) (TVar "f" 1)))))) (TPi "x" (TW (TVar "u" 6) (TVar "v" 5) (TVar "A" 3) (TVar "B" 2)) (TApp (TVar "P" 2) (TVar "x" 0)))))))), []))
    (VLam ("u", TLam "v" (TLam "w" (TLam "A" (TLam "B" (TLam "P" (TLam "ih" (TLam "x" (TWElim (TVar "u" 7) (TVar "v" 6) (TVar "w" 5) (TVar "A" 4) (TVar "B" 3) (TVar "P" 2) (TVar "ih" 1) (TVar "x" 0)))))))), []))

addDef :: Bool -> String -> Expr -> Expr -> StateT Ctx (AccumT [Goal] Error) ()
addDef o s a x = do
  c <- get
  (a', _) <- lift $ elaborateType c a
  let a'' = reduce (env c) a'
  x' <- lift $ elaborate c a'' x
  let x'' = reduce (env c) x'
  if o then do
    let s' = newVar c s
    let c' = c |- Def False s a'' s'
    let c'' = c' |- Def False "#def" a'' x''
    lemma <- lift $ trace "\nCould not generate definitional equality" $ getEquality c'' a []
    let lemma' = reduce (env c'') lemma
    put (c' |- Def False (s ++ ".eq") lemma' (newVar c' (s ++ ".eq")))
  else
    put (c |- Def False s a'' x'')

getEquality :: Ctx -> Expr -> [Value] -> AccumT [Goal] Error Term
getEquality c a e = do
  (a', ta) <- elaborateType c a
  case ta of
    VTypeOmega _ ->
      case a of
        Pi s a'' b -> do
          (a''', _) <- elaborateType c a''
          let a'''' = reduce (env c) a'''
          let s' = newVar c s
          eq <- getEquality (c |- Def True s a'''' s') b (s' : e)
          return $ TPi s a''' eq
        _ -> fail "Pi expression expected"
    VType u -> do
      u' <- getLevelExpr e u
      return $ TEq u' a' (generateEqArg (length e) 0 0) (generateEqArg (length e) 0 1)
    _ -> fail "Type expected"
  where
  getLevelExpr :: MonadFail m => [Value] -> Value -> m Term
  getLevelExpr e' (VStuck (SVar s n)) = TVar s <$> lookupVar e' n
  getLevelExpr e' (VLSucc u) = TLSucc <$> getLevelExpr e' u
  getLevelExpr e' (VLMax u v) = TLMax <$> getLevelExpr e' u <*> getLevelExpr e' v
  getLevelExpr _ _ = fail "Unknown level"
  lookupVar :: MonadFail m => [Value] -> Int -> m Int
  lookupVar (VStuck (SVar _ m) : e') n | n == m = return 0
                                      | otherwise = (+1) <$> lookupVar e' n
  lookupVar _ _ = fail "Unknown variable"
  generateEqArg :: Int -> Int -> Int -> Term
  generateEqArg 0 _ n = TVar "#" n
  generateEqArg l m n = TApp (generateEqArg (l - 1) (m + 1) (n + 1)) (TVar "#" m)