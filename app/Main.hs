module Main (main) where

import Prelude hiding (pi)

import Context
import Control.Monad.Trans.Accum
import Control.Monad.Trans.State
import Data.List (intercalate)
import Def
import Elaborate
import Error
import Quote
import Syntax
import VarNames

main :: IO ()
main = do
  putStrLn (mapError id showState (runAccumT (execStateT test defaultCtx) []))

showState :: (Ctx, [Goal]) -> String
showState (c, g) = showCtx False c ++ "\n\n---\n\n" ++ intercalate "\n\n" (map showGoal g)

showGoal :: Goal -> String
showGoal (n, c, a) = "?" ++ show n ++ "\n" ++ showCtx True c ++ "\n|- " ++ showValue a

showCtx :: Bool -> Ctx -> String
showCtx local = intercalate "\n" . reverse . map (showDef local) . filter ((not local ||) . defLocal)

showDef :: Bool -> Def -> String
showDef local d = defName d ++ " : " ++ showValue (defType d) ++ (if local then "" else " := " ++ showValue (defValue d))

test :: StateT Ctx (AccumT [Goal] Error) ()
test = do
  addDef "id"
    (Pi "u" (Var vLevel) (Pi "a" (App (Var vType) (Var "u")) (pi (Var "a") (Var "a"))))
    (lam (lam (Lam "x" (Var "x"))))
  addDef "const"
    (Pi "u" (Var vLevel) (Pi "a" (App (Var vType) (Var "u")) (pi (Var "a") (pi (Var "a") (Var "a")))))
    (lam (lam (Lam "x" (lam (Var "x")))))
  addDef "quoteClosureTest"
    (Pi "u" (Var vLevel) (Pi "a" (App (Var vType) (Var "u")) (Pi "x" (Var "a") (Pi "b" (pi (pi (Var "a") (Var "a")) (App (Var vType) (Var "u"))) (App (Var "b") (App (App (App (Var "const") (Var "u")) (Var "a")) (Var "x")))))))
    (Lam "u" (Lam "a" (Lam "test" (Lam "b" Hole))))