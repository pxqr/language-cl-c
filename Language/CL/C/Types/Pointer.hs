{-# LANGUAGE TypeOperators #-}
module Language.CL.C.Types.Pointer where

import Language.CL.C.CodeGen.TypeRepr
import Language.CL.C.HOAS.AST
import Language.CL.C.Types.Classes
import Language.CL.C.Types.Scalar

data Pointer a = Pointer Int

instance LangType a => LangType (Pointer a) where
  typeOf = mkPtrTR . typeOf . unsafeUnlift
    where unsafeUnlift :: Pointer a -> a
          unsafeUnlift = error "unsafe unlift pointer type"

instance LangType a => ParamType (Pointer a)
instance LangType a => RetType   (Pointer a)
instance Literal (Pointer a) where
  showLit (Pointer a) = show a
instance LangType a => CLEq (Pointer a) 
instance LangType a => CLOrd (Pointer a)

ref :: ParamType a => Expression a -> Expression (Pointer a)
ref = mkBuiltInUOp "&"

deref :: RetType a => Expression (Pointer a) -> Expression a
deref = mkBuiltInUOp "*"

(*+*) :: LangType a => Expression (Pointer a) -> Expression CLSize -> Expression (Pointer a) 
(*+*) = mkBuiltInBOp "+"

nextP :: LangType a => Expression (Pointer a) -> Expression (Pointer a)
nextP = (*+* 1)

(*-*) :: LangType a => Expression (Pointer a) -> Expression CLSize -> Expression (Pointer a) 
(*-*) = mkBuiltInBOp "-"

nullPtr :: Expression (Pointer a)
nullPtr = mkLit $ Pointer 0
