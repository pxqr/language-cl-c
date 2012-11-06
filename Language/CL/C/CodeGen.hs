-- | This module reexport modules related to code optimization, generation.
module Language.CL.C.CodeGen 
       ( module Language.CL.C.CodeGen.Pretty
       , module Language.CL.C.HOAS.AST
       , mkProgram, showGenError
       ) where

import Language.CL.C.CodeGen.Pretty   (document, showCode)
import Language.CL.C.CodeGen.ProgRepr (Program(Program), TLDecl, identifier) 
import Language.CL.C.HOAS.AST         (mkVar, mkApp, mkFun, mkLit, mkFunarg, Funargs(Funargs)
                                      , StripGlobal, StripError, stripG)

import Data.Dependencies              (Recursion, resolveSD, prune, showError)

data GenError = StripError StripError
              | LinkError  [Recursion TLDecl]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

mkProgram :: StripGlobal a => a -> Either GenError Program
mkProgram f = do ddg <- mapLeft StripError $ stripG f
                 ord <- mapLeft LinkError  $ resolveSD $ prune ddg
                 return $ Program ord

showGenError :: GenError -> String
showGenError (StripError se) = show se
showGenError (LinkError  le) = concatMap (showError identifier) le