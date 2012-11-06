-- | This module reexport modules which gives opportunity to describe C code.
module Language.CL.C 
       ( module Language.CL.C.HOAS.AST    
       , module Language.CL.C.HOAS.Naming  
       , module Language.CL.C.HOAS.Sugar   
       , module Language.CL.C.Types
       ) where

import Language.CL.C.HOAS.AST     (new, ret, while, Expression, StripGlobal)           
import Language.CL.C.HOAS.Naming  (function, cl)
import Language.CL.C.HOAS.Sugar   ((:->), Func, RP, C)
import Language.CL.C.Types   
