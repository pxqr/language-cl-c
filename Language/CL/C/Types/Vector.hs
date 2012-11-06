{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
module Language.CL.C.Types.Vector 
       ( CLVector(..)
       , Vec2, Vec4, Vec8, Vec16
--       , vec2
       ) where

import Language.CL.C.CodeGen.TypeRepr
import Language.CL.C.HOAS.AST
import Language.CL.C.Types.Classes
import Language.CL.C.Types.Scalar

import Data.AdditiveGroup 
import Data.VectorSpace

-- | Multi param type class is necessary because we can't put in context vector type variable when (v :: *) and 
--   we can't define instances for AdditiveGroup, VectorSpace and InnerSpace when (v :: * -> *)
class (Component c, RetType c, ParamType c, RetType (v c), ParamType (v c)) => CLVector (v :: * -> *) (c :: *) where
  replic :: Expression c -> Expression (v c)
  
instance (CLNum c, CLVector v c) => AdditiveGroup (Expression (v c)) where
  zeroV   = replic 0
  (^+^)   = mkBuiltInBOp "+"
  negateV = mkBuiltInUOp "-"

instance (CLNum c, CLVector v c) => VectorSpace (Expression (v c)) where
  type Scalar (Expression (v c)) = Expression c
  (*^) = mkBuiltInBOp "*"

instance (CLNum c, CLVector v c) => InnerSpace (Expression (v c)) where
  (<.>) = curry (mkBuiltInFun "dot")

---------------------------  vecs  ---------------------------------
data Vec2  a = Vec2  a a
data Vec4  a = Vec4  a a a a
data Vec8  a = Vec8  a a a a a a a a
data Vec16 a = Vec16 a a a a a a a a a a a a a a a a
{-
vec2 :: Component a => (a, a) :-> Vec2 a
vec2 (a, b) = mkBuiltInFun (typeName $ typeOf $ liftTV a) (a, b)
   where liftTV :: Expression a -> Vec2 a
         liftTV = undefined
         
vec4 :: Component a => (a, a, a, a) :-> Vec4 a
vec4 (a, b, c, d) = undefined
-}

mkVecTypeRepr :: Component c => VecDim -> v c -> TypeRepr
mkVecTypeRepr dim comp = mkVecTR dim (typeOfComp $ unsafeUnlift comp)
  where unsafeUnlift :: v c -> c
        unsafeUnlift = error "unsafeUnlift vector comp from Vector.hs"

instance Component a => LangType (Vec2 a) where
  typeOf = mkVecTypeRepr Dim2

instance Component a => LangType (Vec4 a) where
  typeOf = mkVecTypeRepr Dim4
  
instance Component a => LangType (Vec8 a) where
  typeOf = mkVecTypeRepr Dim8
  
instance Component a => LangType (Vec16 a) where
  typeOf = mkVecTypeRepr Dim16

instance Component c => CLVector Vec2 c where
  replic = error "replic Vec2" -- vec2 (a, a)

instance Component c => CLVector Vec4 c where 
  replic = error "replic Vec4" -- vec4 (a, a, a, a)
  
instance Component c => CLVector Vec8 c where
  replic = error "replic Vec8"
  
instance Component c => CLVector Vec16 c where
  replic = error "replic Vec16"

instance Component c => RetType (Vec2 c)
instance Component c => RetType (Vec4 c)
instance Component c => RetType (Vec8 c)
instance Component c => RetType (Vec16 c)


instance Component c => ParamType (Vec2 c)
instance Component c => ParamType (Vec4 c)
instance Component c => ParamType (Vec8 c)
instance Component c => ParamType (Vec16 c)
  

{-
class (CLVector v c) => VectorContruction a v c | a -> v where
  vec :: a -> c 

instance Component c => VectorContruction (c, c) Vec2 c where
  vec (a, b) = curry $ mkApp $ mkBuiltInFun $ typeName $ typeOf (undefined :: Vec2 c)
-}