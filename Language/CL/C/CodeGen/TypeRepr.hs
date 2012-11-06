-- | NOTE: polymorphic types and functions doesn't have representation.
module Language.CL.C.CodeGen.TypeRepr 
       ( AtomT(..), CompT(..), ScaT(..), VecDim(..), LangT(..)
       , dimToNum
       , TypeRepr(typeRepr), mkAtomTR, mkCompTR, mkVecTR, mkPtrTR
       , mkTypeHash
       ) where


import Language.CL.C.CodeGen.Pretty

import Text.PrettyPrint.HughesPJ (text, int, (<+>), (<>))


-- | Basic atom types that can not parametrize vector type.
data AtomT = VoidT  | BoolT 
           deriving (Eq, Ord)

-- | Component atom types that can parametrize vector type.
data CompT = CharT  | UCharT 
           | ShortT | UShortT 
           | IntT   | UIntT 
           | LongT  | ULongT 
           | FloatT | DoubleT
           | HalfT  
             deriving (Eq, Ord)
                      
-- | Scalar types are all atomic types.
data ScaT  = AtomT AtomT
           | CompT CompT
             deriving (Eq, Ord)

data VecDim = Dim2 | Dim4 | Dim8 | Dim16
            deriving (Eq, Ord)

-- | Language type that can represent all possible monomorphic type.
data LangT = ScaT ScaT
           | VecT VecDim CompT
           | PtrT LangT
             deriving (Eq, Ord)

--escapePtr :: TypeRepr -> a

-- | Wrapper for 'LangT'. Has an r in the end to do not mix up with Data.Typeable.TypeRep
newtype TypeRepr = TypeRepr { typeRepr :: LangT }
                   deriving (Eq, Ord)

mkAtomTR :: AtomT -> a -> TypeRepr
mkAtomTR con _ = TypeRepr $ ScaT $ AtomT con

mkCompTR :: CompT -> a -> TypeRepr
mkCompTR con _ = TypeRepr $ ScaT $ CompT con

mkVecTR :: VecDim -> CompT -> TypeRepr
mkVecTR dim compt = TypeRepr $ VecT dim compt

mkPtrTR :: TypeRepr -> TypeRepr
mkPtrTR t = TypeRepr $ PtrT $ typeRepr t

dimToNum :: Num a => VecDim -> a
dimToNum Dim2  = 2
dimToNum Dim4  = 4
dimToNum Dim8  = 8
dimToNum Dim16 = 16

instance Pretty AtomT where
  document VoidT   = text "void"
  document BoolT   = text "bool"

instance Pretty CompT where
  document CharT   = text "char"
  document UCharT  = text "uchar"
  document ShortT  = text "short"
  document UShortT = text "ushort"
  document IntT    = text "int"
  document UIntT   = text "uint"
  document LongT   = text "long"
  document ULongT  = text "ulong"
  document FloatT  = text "float"
  document DoubleT = text "double"
  document HalfT   = text "half"

instance Pretty ScaT where
  document (AtomT atomT) = document atomT
  document (CompT compT) = document compT

instance Pretty LangT where
  document (VecT dim compT) = document compT <> int (dimToNum dim)
  document (ScaT     scaT ) = document scaT
  document (PtrT     tpe  ) = document tpe   <+> text "*"

instance Pretty TypeRepr where
  document = document . typeRepr

-- | Returns a unique hash of a given type that can be safely used in function name mangling.
--   Hash string can contain only letters.
newtype TypeHash = TypeHash { typeHash :: LangT }

instance Pretty TypeHash where
  document = typeUHash . typeHash
     where typeUHash (PtrT a)  = text "pointer_" <> typeUHash a
           typeUHash a  = document a

mkTypeHash :: TypeRepr -> String
mkTypeHash = showCode . TypeHash . typeRepr