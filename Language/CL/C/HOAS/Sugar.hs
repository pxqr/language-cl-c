{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, UndecidableInstances #-}
module Language.CL.C.HOAS.Sugar  
       ((:->), Func, RP, C
       ) where

import Language.CL.C.HOAS.AST
import Language.CL.C.Types.Classes

type C = Expression

class  (ParamType a, RetType a) => RP a
instance (ParamType a, RetType a) => RP a

type Func a b = Expression a -> Expression b

type family a :-> b
type instance (a, b) :-> c = (Expression a, Expression b) -> Expression c
type instance (a, b, c) :-> d = (Expression a, Expression b, Expression c) -> Expression d
type instance (a, b, c, d) :-> e = (Expression a, Expression b, Expression c, Expression d) -> Expression e
type instance (a, b, c, d, e) :-> f = (Expression a, Expression b, Expression c, Expression d, Expression e) -> Expression f
type instance (a, b, c, d, e, f) :-> g = (Expression a, Expression b, Expression c, 
                                          Expression d, Expression e, Expression f) -> Expression g
