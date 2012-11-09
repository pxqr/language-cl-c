{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Language.CL.C.CodeGen.ProgRepr
       ( Identifier
       , Expr(..), Statement(..), CompoundStatement(..)
       , Parameter(..), ParList(..), Signature(..), Funargs(..), Procedure(..)
       , TLDecl(..), identifier
       , Program(..)
       ) where

--import Language.CL.C.CodeGen.Mangling
import Language.CL.C.CodeGen.TypeRepr
import Language.CL.C.CodeGen.Pretty

import Data.Monoid               (Monoid)
import Data.List                 (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, empty, text, char, nest, punctuate, parens, vcat, hcat, hsep
                                 , equals, semi, comma, space, lbrace, rbrace
                                 , ($+$), ($$), (<+>), (<>))

import Data.Function (on)
import Data.Ord (comparing)

type Identifier = String

data Expr = Funcall Signature [Expr] 
          | FuncallB Identifier [Expr]
          | UnaryOp Identifier Expr
          | BinaryOp  Identifier Expr Expr
          | TernaryOp Expr Expr Expr
          | Variable Identifier
          | Literal String

data Statement = Definition TypeRepr Identifier (Maybe Expr)
               | Sequence   Expr
               | Assignment Expr Expr
               | WhileLoop  Expr CompoundStatement
               | Return (Maybe Expr)

data CompoundStatement = CompoundStatement [Statement]

data Parameter = Parameter { parType :: TypeRepr
                           , parIden :: Identifier
                           } 
                 
instance Eq Parameter where
  (==) = (==) `on` parType

instance Ord Parameter where
  compare = comparing parType

data ParList   = ParList   [Parameter]
                 deriving (Eq, Ord)

newtype Funargs = Funargs [Signature]
                deriving (Eq, Ord, Monoid)
                        
data Signature = Signature { retType :: TypeRepr
                           , procID  :: Identifier 
                           , parList :: ParList
                           , funargs :: Funargs
                           } deriving (Eq, Ord)

data Procedure = Procedure { signature :: Signature 
                           , bodyBlock :: CompoundStatement
                           } 
                 
-- make assumpition that procedure are unique identified by signature
instance Eq Procedure where
  (==) = (==) `on` signature

instance Ord Procedure where
  compare = comparing signature

data TLDecl = ProcDef Procedure 
              deriving (Eq, Ord)

identifier :: TLDecl -> Identifier
identifier (ProcDef proc) = procID $ signature proc

data Program   = Program [TLDecl]

-- | Allows parametric polymorphism. Do not escape name (prefix of mangled name) because 
--   incorrect names are rejected by stripping stage.
mangleFunName :: Signature -> Doc
mangleFunName (Signature retT name (ParList parsT) (Funargs fargs)) = text name <-> typeSuffix <-> funargSuffix
  where typeSuffix   = let plcomma = delimeter <> text "and" <> delimeter
                           parSuff = hcat $ punctuate plcomma $ map (text . mkTypeHash . parType) parsT 
                           retSuff = text $ mkTypeHash retT
                       in text "_from_" <-> parSuff <-> text "_to_" <-> retSuff
        funargSuffix = hcat $ punctuate delimeter $ map mangleFunName fargs
        p <-> q      = p <> delimeter <> q
        delimeter    = char '_'
        

instance Pretty Parameter where
  document (Parameter tpe name) = document tpe <+> text name

instance Pretty ParList where
  document (ParList xs) = parens $ hsep $ punctuate comma $ map document xs

argList :: Pretty a => [a] -> Doc
argList = hsep . punctuate comma . map document 

instance Pretty Expr where
  document (Literal  name)      = text name
  document (Variable name)      = text name
  document (UnaryOp name a)     = parens (text name <+> document a)
  document (BinaryOp name p q)  = parens (document p <+> text name <+> document q)
  document (TernaryOp cond p q) = parens (document cond <+> text "?" <+> document p <+> text ":" <+> document q)
  document (Funcall sig args)   = mangleFunName sig <> parens (argList args)
  document (FuncallB iden args) = text iden <> parens (argList args)
  
instance Pretty Statement where
  document (Definition tpe name expr) = document tpe <+> text name 
                                        <> maybe empty ((space <> equals <+>) . document) expr <> semi 
  document (Sequence expr)            = document expr <> semi
  document (Return e)                 = text "return" <+> document e <> semi
  document (Assignment p expr)        = document p <+> equals  <+> document expr <> semi
  document (WhileLoop cond block)     = text "while" <> parens (document cond) $+$ document block

-- | Since human readable code is not a main priority offset is constant.
indentOffset :: Int
indentOffset = 2



instance Pretty CompoundStatement where
  document (CompoundStatement sts) = lbrace
                                     $+$ nest indentOffset (foldl ($+$) empty $ map document sts) 
                                     $+$ rbrace

instance Pretty Signature where
  document sig@(Signature ret _ pl _) = document ret <+> mangleFunName sig <> document pl
                
instance Pretty Procedure where
  document (Procedure sig block) = document sig $$ document block

instance Pretty TLDecl where
  document (ProcDef proc) = document proc

instance Pretty Program where
  document (Program tlds) = vcat $ intersperse space $ map document tlds