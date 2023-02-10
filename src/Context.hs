module Context (Def(..), Ctx, (|-), env, getVar, newVar) where

import Syntax

data Def = Def { defLocal :: Bool, defName :: String, defType :: Value, defValue :: Value }

type Ctx = [Def]

(|-) :: Ctx -> Def -> Ctx
c |- d = d : c

env :: Ctx -> Env
env = map defValue

getVar :: MonadFail m => String -> Ctx -> m (Def, Int)
getVar = getVar' 0
  where
  getVar' _ s [] = fail $ "Variable " ++ s ++ " not in scope"
  getVar' n s (d : c) | defName d == s = return (d, n)
                      | otherwise = getVar' (n + 1) s c

newVar :: Ctx -> String -> Value
newVar c s = VStuck (SVar s (length c)) Nothing