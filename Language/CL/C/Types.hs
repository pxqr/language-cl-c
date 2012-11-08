-- | This module just reexport modules related to EDSL types.
module Language.CL.C.Types   
       ( module Language.CL.C.Types.Scalar
       , module Language.CL.C.Types.Vector
       , module Language.CL.C.Types.Pointer
       , module Language.CL.C.Types.Classes
--       , CLFunctor(..)
       ) where

import Language.CL.C.Types.Scalar
import Language.CL.C.Types.Vector
import Language.CL.C.Types.Pointer
import Language.CL.C.Types.Classes

import Language.CL.C.HOAS.AST ()

{-
class CLFunctor f where
  fmap :: (ParamType a, RetType a, RetType b, ParamType b, ParamType (f a), RetType (f a)) => 
          (Expression a -> Expression b) -> (Expression (f a) -> Expression (f b))

instance CLFunctor Pointer where
  fmap op = mkApp $ mkFun "fmap" (Funargs [mkFunarg op]) $ \ ptr -> 
     ret $ ref $ op $ deref ptr
-}