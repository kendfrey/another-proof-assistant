module VarNames (
  vBool,
  vBoolElim,
  vEmpty,
  vEmptyElim,
  vEq,
  vEqElim,
  vFalse,
  vLevel,
  vLMax,
  vLSucc,
  vPair,
  vRefl,
  vSigma,
  vSigmaElim,
  vStar,
  vSup,
  vTrue,
  vType,
  vUnit,
  vUnitElim,
  vW,
  vWElim,
  ) where

vBool :: String
vBool = "Bool"

vBoolElim :: String
vBoolElim = "Bool.elim"

vEmpty :: String
vEmpty = "Empty"

vEmptyElim :: String
vEmptyElim = "Empty.elim"

vEq :: String
vEq = "Eq"

vEqElim :: String
vEqElim = "Eq.elim"

vFalse :: String
vFalse = "false"

vLevel :: String
vLevel = "Level"

vLMax :: String
vLMax = "lMax"

vLSucc :: String
vLSucc = "lSucc"

vPair :: String
vPair = "pair"

vRefl :: String
vRefl = "refl"

vSigma :: String
vSigma = "Sigma"

vSigmaElim :: String
vSigmaElim = "Sigma.elim"

vStar :: String
vStar = "star"

vSup :: String
vSup = "sup"

vTrue :: String
vTrue = "true"

vType :: String
vType = "Type"

vUnit :: String
vUnit = "Unit"

vUnitElim :: String
vUnitElim = "Unit.elim"

vW :: String
vW = "W"

vWElim :: String
vWElim = "W.elim"