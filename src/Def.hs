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

addDef :: String -> Expr -> Expr -> StateT Ctx (AccumT [Goal] Error) ()
addDef s a x = do
  c <- get
  d <- lift $ checkDef c
  put (c |- d)
    where
    checkDef :: Ctx -> AccumT [Goal] Error Def
    checkDef c = do
      (a', _) <- elaborateType c a
      let a'' = reduce (env c) a'
      x' <- elaborate c a'' x
      let x'' = reduce (env c) x'
      return $ Def False s a'' x''