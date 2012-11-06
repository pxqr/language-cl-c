-- | We can avoid a lot of boilerplate in this module, but it needs XUndecidableInstances and XIncoherentInstances...
{-# LANGUAGE FlexibleInstances #-}
module Language.CL.C.Types.Scalar where

import Language.CL.C.CodeGen.TypeRepr
import Language.CL.C.Types.Classes
import Language.CL.C.HOAS.AST

import Data.Monoid    (mempty)
import Prelude hiding (Eq(..), Ord(..))

---------------------- eq & ord relation ----------------
class (ParamType a) => CLEq a where
  (==) :: Expression a -> Expression a -> Expression CLBool
  (==) = mkBuiltInBOp "=="
  (/=) :: Expression a -> Expression a -> Expression CLBool
  (/=) = mkBuiltInBOp "!="  

-- | use 'signum' istead of 'compare'
class (ParamType a, RetType a) => CLOrd a where
  (<) :: Expression a -> Expression a -> Expression CLBool
  (<) = mkBuiltInBOp "<"
  
  (>=) :: Expression a -> Expression a -> Expression CLBool
  (>=) = mkBuiltInBOp ">="
  
  (>) :: Expression a -> Expression a -> Expression CLBool
  (>) = mkBuiltInBOp ">"
  
  (<=) :: Expression a -> Expression a -> Expression CLBool
  (<=) = mkBuiltInBOp "<="
  
  max :: Expression a -> Expression a -> Expression a
  max = curry $ mkBuiltInFun "max"
  
  min :: Expression a -> Expression a -> Expression a
  min = curry $ mkBuiltInFun "min"

---------------------- numeric scalars ----------------------
class (ParamType a, RetType a, Literal a, CLOrd a) => CLNum a where
  packNum   :: Integer -> a

instance (CLNum a) => Num (Expression a) where
  (+) = mkBuiltInBOp "+"
  (*) = mkBuiltInBOp "*"
  (-) = mkBuiltInBOp "-"
  negate = mkBuiltInUOp "-"
  abs = mkBuiltInFun "abs"
  fromInteger = mkLit . packNum
  
  signum = mkApp $ mkFun "sugnum" mempty $ \ x -> 
    ret (((x < 0) ? 1) $ 0)

class (ParamType a, RetType a, Literal a, CLNum a) => CLFractional a where
  packFractional :: Rational -> a

instance (CLFractional a, CLNum a) => Fractional (Expression a) where
  (/) = mkBuiltInBOp "/"
  fromRational = mkLit . packFractional

--------------------- atom types    -----------------------
newtype CLVoid = CLVoid ()
newtype CLBool   = CLBool Bool       

newtype CLChar   = CLChar Integer
newtype CLUChar  = CLUChar  Integer

newtype CLShort  = CLShort  Integer
newtype CLUShort = CLUShort Integer

newtype CLInt    = CLInt    Integer
newtype CLUInt   = CLUInt   Integer

newtype CLLong   = CLLong   Integer
newtype CLULong  = CLULong  Integer

newtype CLFloat  = CLFloat  Float
newtype CLDouble = CLDouble Double
newtype CLHalf   = CLHalf   ()

newtype CLSize   = CLSize   Integer
newtype CLPtrDiff = CLPtrDiff Int

instance BoolLike CLBool 
(.&&.) :: Expression CLBool -> Expression CLBool -> Expression CLBool
(.&&.) = mkBuiltInBOp "&&"

------------------ LangType instances ------------------
instance LangType CLVoid where
  typeOf = mkAtomTR VoidT

instance LangType CLBool where
  typeOf = mkAtomTR BoolT

instance LangType  CLChar where
  typeOf = mkCompTR CharT

instance LangType CLUChar where
  typeOf = mkCompTR UCharT

instance LangType CLShort where
  typeOf = mkCompTR ShortT
  
instance LangType CLUShort where
  typeOf = mkCompTR UShortT

instance LangType CLInt where
  typeOf = mkCompTR IntT

instance LangType CLUInt where
  typeOf = mkCompTR UIntT

instance LangType CLLong where
  typeOf = mkCompTR UIntT

instance LangType CLULong where
  typeOf = mkCompTR ULongT

instance LangType CLFloat where
  typeOf = mkCompTR FloatT

instance LangType CLDouble where
  typeOf = mkCompTR DoubleT
  
instance LangType CLHalf where
  typeOf = mkCompTR HalfT
  
-- -----------------  Warning! Make constr for size and ptrdiff------------
instance LangType CLSize where
  typeOf = mkCompTR ULongT
  
instance LangType CLPtrDiff where
  typeOf = mkCompTR LongT
  
----------------- Literal instances ------------------
instance Literal CLBool where
  showLit (CLBool True)  = "true"
  showLit (CLBool False) = "false"

true :: Expression CLBool
true = mkLit $ CLBool True

false :: Expression CLBool
false = mkLit $ CLBool False

instance Literal CLChar where
  showLit (CLChar a) = show a

instance Literal CLUChar where
  showLit (CLUChar a) = show a

instance Literal CLShort where
  showLit (CLShort a) = show a

instance Literal CLUShort where
  showLit (CLUShort a) = show a

instance Literal CLInt where
  showLit (CLInt a) = show a

instance Literal CLUInt where
  showLit (CLUInt a) = show a

instance Literal CLLong where
  showLit (CLLong a) = show a

instance Literal CLULong where
  showLit (CLULong a) = show a

instance Literal CLFloat where
  showLit (CLFloat a) = show a

instance Literal CLDouble where
  showLit (CLDouble a) = show a

instance Literal CLSize where
  showLit (CLSize a)  = show a
----------------- ParamType instances ---------------
-- NOTE! instance ParamType CLVoid forbidden
instance ParamType CLBool 
instance ParamType CLChar 
instance ParamType CLUChar
instance ParamType CLShort
instance ParamType CLUShort
instance ParamType CLInt
instance ParamType CLUInt
instance ParamType CLLong
instance ParamType CLULong
instance ParamType CLFloat
instance ParamType CLDouble
instance ParamType CLHalf
instance ParamType CLSize

----------------- RetType instances  ---------------
instance RetType   CLVoid
instance RetType   CLBool
instance RetType   CLChar
instance RetType   CLUChar
instance RetType   CLShort
instance RetType   CLUShort
instance RetType   CLInt
instance RetType   CLUInt
instance RetType   CLLong
instance RetType   CLULong
instance RetType   CLFloat
instance RetType   CLDouble
instance RetType   CLHalf
instance RetType   CLSize
---------------- CLNum instances     ---------------
instance CLNum CLChar where
  packNum = CLChar

instance CLNum CLUChar where
  packNum = CLUChar

instance CLNum CLShort where
  packNum = CLShort

instance CLNum CLUShort where
  packNum = CLUShort
  
instance CLNum CLInt where
  packNum = CLInt
  
instance CLNum CLUInt where
  packNum = CLUInt
  
instance CLNum CLLong where
  packNum = CLLong
  
instance CLNum CLULong where
  packNum = CLULong 
  
  
instance CLNum CLFloat where
  packNum = CLFloat . fromInteger
  
instance CLNum CLDouble where
  packNum = CLDouble . fromInteger
  
--instance CLNum CLHalf where
--o  packNum = CLHalf

instance CLNum CLSize where
  packNum = CLSize
----------------- CLFractional instances ------------
instance CLFractional CLDouble where
  packFractional = CLDouble . fromRational
  
instance CLFractional CLFloat where
  packFractional = CLFloat . fromRational

---------------- CLEq instances      ---------------
instance CLEq CLBool
instance CLEq CLChar
instance CLEq CLUChar
instance CLEq CLShort
instance CLEq CLUShort
instance CLEq CLInt
instance CLEq CLUInt
instance CLEq CLLong
instance CLEq CLULong
instance CLEq CLFloat
instance CLEq CLHalf
instance CLEq CLDouble

----------------- CLOrd instances    ---------------
instance CLOrd CLBool
instance CLOrd CLChar
instance CLOrd CLUChar
instance CLOrd CLShort
instance CLOrd CLUShort
instance CLOrd CLInt
instance CLOrd CLUInt
instance CLOrd CLLong
instance CLOrd CLULong
instance CLOrd CLFloat
instance CLOrd CLDouble
instance CLOrd CLHalf
instance CLOrd CLSize
---------------- Component instances ---------------

instance Component CLChar  where
  typeOfComp _ = CharT

instance Component CLUChar where
  typeOfComp _ = UCharT
  
instance Component CLShort where
  typeOfComp _ = ShortT
  
instance Component CLUShort where
  typeOfComp _ = UShortT
  
instance Component CLInt where
  typeOfComp _ = IntT
  
instance Component CLUInt where
  typeOfComp _ = UIntT
  
instance Component CLLong where
  typeOfComp _ = LongT
  
instance Component CLULong where
  typeOfComp _ = ULongT

instance Component CLFloat where
  typeOfComp _ = FloatT

instance Component CLDouble where
  typeOfComp _ = DoubleT
