{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, UndecidableInstances #-}
module Language.CL.C.HOAS.Sugar  
       ((:->), Func, Proc, Procedure, RP, C
       ) where

import Language.CL.C.HOAS.AST (Function, Expression)
import Language.CL.C.Types.Classes

class  (ParamType a, RetType a) => RP a
instance (ParamType a, RetType a) => RP a

type C = Expression

type Func a b = Expression a -> Expression b
type Proc b = Func () b
type Procedure b = Function () b


type family a :-> b
type instance () :-> a = () -> C a
type instance (a, b) :-> c = (C a, C b) -> C c
type instance (a, b, c) :-> d = (C a, C b, C c) -> C d
type instance (a, b, c, d) :-> e = (C a, C b, C c, C d) -> C e
type instance (a, b, c, d, e) :-> f = (C a, C b, C c, C d, C e) -> C f
type instance (a, b, c, d, e, f) :-> g = (C a, C b, C c, C d, C e, C f) -> C g
