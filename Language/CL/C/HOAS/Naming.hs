-- TODO: remove mkApp, unsafeUnApp
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Language.CL.C.HOAS.Naming (function, cl) where

import Language.CL.C.HOAS.AST
import Language.CL.C.Types.Classes

import Language.Haskell.TH

import Control.Monad (join)
import Data.Monoid (mempty)

-- | 'function' should be used for user-defined functions.
--   UNSAFE! should be used only inside cl splicing!
function :: (Parameters a, RetType b) => (a -> Language.CL.C.HOAS.AST.Body b) -> (a -> Expression b)
function def = mkApp $ mkFun (error "function") mempty def


cl :: Q [Dec] -> Q [Dec]
cl quote = join (mapM naming `fmap` quote)



naming :: Dec -> Q Dec
naming (FunD name [Clause funargs (NormalB expr) []]) = do
  flist <- mkFunargList funargs
  let newBody = AppE (AppE (VarE 'mkFunarged) flist) (AppE (mkEntry name) expr)
  return $ FunD name [Clause funargs (NormalB newBody) []]

naming (ValD (VarP name) (NormalB expr) _) = 
  return $ ValD (VarP name) (NormalB (AppE (mkEntry name) expr)) []
  
naming (FunD name _)         = fail (show name ++ " -- clause should be only one!")
naming a = return a -- leave other declarations unchanged

mkFunargList :: [Pat] -> Q Exp
mkFunargList args = do fars <- mapM f args
                       return $ AppE (ConE 'Funargs) (ListE fars)
  where f (VarP v) = return $ AppE (VarE 'mkFunarg) (VarE v)
        f _        = fail "Pattern matching is not allowed!"
        
mkFunarged :: (Parameters a, RetType b) => Funargs -> (a -> Expression b) -> (a -> Expression b)
mkFunarged funargs fun = mkApp (addFunargs funargs $ unsafeUnApp fun)

mkEntry :: Name -> Exp
mkEntry identificator = AppE (VarE 'mkNamed) (LitE (StringL (show identificator)))

mkNamed :: (Parameters a, RetType b) => String -> (a -> Expression b) -> (a -> Expression b)
mkNamed ident fun = mkApp (setName ident $ unsafeUnApp fun)

