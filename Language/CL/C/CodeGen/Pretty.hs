-- | This module defines convertion from 'Raw' concrete syntax tree to text of 'Doc'.
module Language.CL.C.CodeGen.Pretty 
       ( document, showCode, Pretty
       ) where

import Text.PrettyPrint.HughesPJ (Doc, render, empty)

-- | Convert concrete syntax tree to 'Doc'.
class Pretty a where
  document :: a -> Doc
  
-- | Convert concrete syntax tree to well formatted plain text.
showCode :: Pretty a => a -> String
showCode = render . document


instance Pretty a => Pretty (Maybe a) where
  document = maybe empty document
  
