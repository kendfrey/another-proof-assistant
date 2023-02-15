module Main (main) where

import Prelude hiding (pi)

import Context
import Control.Monad.Trans.Accum
import Control.Monad.Trans.State
import Data.List (intercalate)
import Def
import Elaborate
import Error
import Parser
import Quote
import Syntax
import System.Environment
import Text.Megaparsec.Error

main :: IO ()
main = do
  file <- head <$> getArgs
  parsed <- parseFile file
  case parsed of
    Right program -> putStrLn (mapError id showState (runAccumT (execStateT program defaultCtx) []))
    Left err -> putStrLn (errorBundlePretty err)

showState :: (Ctx, [Goal]) -> String
--showState (c, g) = showCtx False c ++ "\n\n---\n\n" ++ intercalate "\n\n" (map showGoal g)
showState (_, g) = intercalate "\n\n" (map showGoal g)

showGoal :: Goal -> String
showGoal (s, c, a) = "?" ++ s ++ "\n" ++ showCtx True c ++ "\n|- " ++ showValue a

showCtx :: Bool -> Ctx -> String
showCtx local = intercalate "\n" . reverse . map (showDef local) . filter ((not local ||) . defLocal)

showDef :: Bool -> Def -> String
showDef local d = defName d ++ " : " ++ showValue (defType d) ++ (if local then "" else " := " ++ showDefValue (defValue d)) ++ ";"

showDefValue :: Value -> String
showDefValue (VStuck _ (Just x)) = showDefValue x
showDefValue x = showValue x