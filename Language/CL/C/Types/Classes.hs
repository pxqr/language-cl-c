-- | This module defines classes used as basic constraits for AST.
{-# LANGUAGE FlexibleInstances #-}
module Language.CL.C.Types.Classes 
       ( LangType(..), ParamType, RetType
       , Literal(..), BoolLike, Component(..)
       ) where

import Language.CL.C.CodeGen.TypeRepr 

-- | Instances of 'LangType' SHOULD NOT use the only argument because it can be 'undefined'. 
--   Each type occured in AST should have type representation.
class LangType a where
  typeOf :: a -> TypeRepr

-- | Types which can be passed as an argument in a procedure.
class LangType a => ParamType a 

-- | Types which can be returned from a procedure.
class LangType a => RetType a  

-- | Type which can be returned from and passed in a procedire.
--class Transf a 

--instance (ParamType a, RetType a) => Transf a

-- | Types which have literal representation.
class Literal a where
  showLit :: a -> String
  
-- | Types which have associated boolean operations.
class BoolLike a 

class (ParamType c, RetType c) => Component c where
  typeOfComp :: c -> CompT
--class AtomType a
