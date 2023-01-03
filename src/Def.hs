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

addDef :: Bool -> String -> Expr -> Expr -> StateT Ctx (AccumT [Goal] Error) ()
addDef o s a x = do
  c <- get
  (a', _) <- lift $ elaborateType c a
  let a'' = reduce (env c) a'
  x' <- lift $ elaborate c a'' x
  let x'' = reduce (env c) x'
  if o then do
    let s' = newVar c s
    let c' = (c |- Def False s a'' s')
    -- TODO this needs to be an equality
    let lemma = VPi VLevel ("__u", TPi "P" (TPi "_" (TVar "#" 3) (TType (TVar "__u" 1))) (TPi "_" (TApp (TVar "P" 0) (TVar "#" 3)) (TApp (TVar "P" 1) (TVar "#" 3))), [s', x'', a''])
    put (c' |- Def False (s ++ ".def") lemma (newVar c' (s ++ ".def")))
  else
    put (c |- Def False s a'' x'')