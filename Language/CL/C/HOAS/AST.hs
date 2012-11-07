-- | Warning: Funargs are supported partially.
{-# LANGUAGE GADTs, RankNTypes, KindSignatures, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls #-}
module Language.CL.C.HOAS.AST  
       ( Expression, Function, FunctionE, Parameters
       -- * 'Expression' abstract data type constructors
       , mkLit, mkVar, mkApp, unsafeUnApp, mkFun
       , mkFunarg, Funargs(Funargs), addFunargs, setName
       -- * Builtins                                                  
       , mkBuiltInFun, mkBuiltInUOp, mkBuiltInBOp 
       -- * Block
       , Block, Body
       -- * Basic operations
       , new, while, (?), assign, ret, retVoid, CLVoid
       -- * Stripping
       , stripG, StripGlobal, StripError
       ) where

import Language.CL.C.Types.Classes
import Language.CL.C.CodeGen.TypeRepr
import Language.CL.C.CodeGen.ProgRepr

import Data.Char    (ord)
import Data.Monoid  ((<>))
import Data.Dependencies
import Data.Scope

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Identity
import Control.Monad.Error

-- | 'FunStyle' defines way in which AST related to application will be rendered.
data Spelling  = BOP Identifier       --  represents unary operators
               | UOP Identifier       --  represents binary operators
               | TOP 
               | Ordinary Identifier  --  represents ordinary functions
                 deriving (Show, Read, Eq)

data Domain a = Embedded Identifier Funargs a 
              | BuiltIn  Spelling


class Parameters a where
  list      :: a -> DDF TLDecl [Expr]
  mkParams  :: Scope (Claim r) a  
  paramList :: a -> [TypeRepr]

data FunctionE :: * -> * -> * where  
  Fun :: (Parameters a, RetType b) => Domain (Body b) -> FunctionE a (Expression b)

data Expression :: * -> * where  
  Lit :: Literal a  => a -> Expression a
  Var :: LangType a => Index -> Expression a
  App :: FunctionE a (Expression b) -> a -> Expression b


type Function a b = FunctionE a (Expression b)

mkFunarg :: (Parameters a, RetType b) => (a -> Expression b) -> Signature
mkFunarg = mkSignature . unsafeUnApp

-- unique type function to ensure that function are included
-- !!! actually i don't remember that it ^^ means

-- | Makes an ordinary literal.
mkLit :: Literal a => a -> Expression a
mkLit = Lit

-- | 'mkVar' is the only way to create a new variable. Typesafety are provided by signature.
mkVar :: LangType a => Expression a -> Block r a
mkVar e = do index  <- freshIndex
             add    $ Def index e
             return $ Var index

-- | Makes an ordinary application.
mkApp :: (Parameters a, RetType b) => Function a b -> a -> Expression b
mkApp = App

-- | UNSAFE!
unsafeUnApp :: (Parameters a) => (a -> Expression b) -> Function a b
unsafeUnApp fun = case fun $ error "unsafeUnApp:1" of
                    App (Fun d) _ -> Fun d
                    _             -> error "unsafeUnApp:2"

mkParam :: ParamType a => Block r a
mkParam = Var <$> freshIndex

-- unlift types from Expression
typeOfExpr :: LangType a => Expression a -> TypeRepr
typeOfExpr = typeOf . unliftExpression
    where unliftExpression :: Expression a -> a
          unliftExpression = error "typeOfExpr"

instance Parameters () where
  list a      = return []
  mkParams    = return ()
  paramList _ = []

instance (LangType a, ParamType a) => Parameters (Expression a) where
  list a      = return <$> strip a
  mkParams    = mkParam
  paramList a = [typeOfExpr a]

instance (ParamType a, ParamType b) => Parameters (Expression a, Expression b) where
  list (a, b) = do a'  <- strip a 
                   b'  <- strip b
                   return [a', b']
  mkParams = liftM2 (,) mkParams mkParams
  paramList ~(a, b) = [typeOfExpr a, typeOfExpr b]

instance (ParamType a, ParamType b, ParamType c) => Parameters (Expression a, Expression b, Expression c) where
  list (a, b, c) = do a' <- strip a
                      b' <- strip b
                      c' <- strip c
                      return [a', b', c']
  mkParams = liftM3 (,,) mkParams mkParams mkParams
  paramList ~(a, b, c) = [typeOfExpr a, typeOfExpr b, typeOfExpr c]

instance (ParamType a, ParamType b, ParamType c, ParamType d) => 
         Parameters (Expression a, Expression b, Expression c, Expression d) where
  list (a, b, c, d) = do a' <- strip a
                         b' <- strip b
                         c' <- strip c
                         d' <- strip d
                         return [a', b', c', d']
  mkParams = liftM4 (,,,) mkParams mkParams mkParams mkParams
  paramList ~(a, b, c, d) = [typeOfExpr a, typeOfExpr b, typeOfExpr c, typeOfExpr d]


-- | Makes a function in typesafe way. Typesafety provided by signature. It lift up parameters.  
mkFun :: (Parameters a, RetType b) => Identifier -> Funargs -> (a -> Body b) -> FunctionE a (Expression b)
mkFun name fs def = Fun $ Embedded name fs $ mkParams >>= def
                                          
setName :: Identifier -> Function a b -> Function a b
setName name (Fun (Embedded _ body fs)) = Fun $ Embedded name body fs
setName _    fun                        = fun

addFunargs :: Funargs -> Function a b -> Function a b
addFunargs funs (Fun (Embedded name fs body)) = Fun $ Embedded name (funs <> fs) body 
addFunargs _    fun                           = fun

-- | NOTE: You should provide correct type in signature for each builtin, because it impossible to check it! (how???)
--   Builtin operators will be surrounded by parens, so you shouldn't provide operator precedence.

-- | Make a builtin function. Appearence of funcall of such func will be ordinary, as for auxilary functions. 
--   NOTE: Mangling won't be applied to function identifier because mangled name won't match target names! 
mkBuiltInFun :: (Parameters a, RetType b) => Identifier -> a -> Expression b
mkBuiltInFun name = mkApp (Fun $ BuiltIn $ Ordinary name)

-- | Make a builtin unary prefix operator. 
--   NOTE: Mangling won't be applied! HOW? WHY? FOR WHAT?
mkBuiltInUOp :: (ParamType a, RetType b) => Identifier -> Expression a -> Expression b
mkBuiltInUOp name = mkApp (Fun $ BuiltIn $ UOP name)

-- | Make a builtin infix binary operator.
--   NOTE: Mangling won't be applied! HOW? WHY? FOR WHAT?
mkBuiltInBOp :: (ParamType a, ParamType b, RetType c) => Identifier -> Expression a -> Expression b -> Expression c
mkBuiltInBOp name lhs rhs = mkApp (Fun $ BuiltIn $ BOP name) (lhs, rhs)

data Claim r = Ret (Maybe (Expression r))
             | forall a. LangType a => Def Index (Expression a) 
             | forall a. BoolLike a => While (Expression a) (Body r)
             | forall a. LangType a => Assign (Expression a) (Expression a)

-- | /r/ is return type, /a/ is definition type. 
--   /r/ type variable helpfull to restrict return type to only one,
--   thus we can't return expressions with distinct types in one scope.
type Block r a = Scope (Claim r) (Expression a)
type Body r    = Scope (Claim r) ()

-- | 'new' is the same as mkVar and defined for convenience.
new :: LangType a => Expression a -> Block r a
new = mkVar

-- | 
ret :: LangType r => Expression r -> Body r
ret e = add $ Ret $ Just e
-- or maybe this?
--  v <- new e
--  add $ Ret v

data CLVoid 

retVoid :: Body CLVoid
retVoid = add $ Ret $ Nothing

-- | While loop.
while :: BoolLike b => Expression b -> Body r -> Body r
while cond blk  = add $ While cond blk

(?) :: (ParamType a, ParamType b, RetType a, BoolLike b) => Expression b -> Expression a -> Expression a -> Expression a
(?) cond right left = mkApp (Fun $ BuiltIn TOP) (cond, right, left)

assign :: LangType a => Expression a -> Expression a -> Body r
assign p q = add $ Assign p q

------------------------------------- Stripping ---------------------------------------------
------------------------------ TODO: PRETIFY THIS PART!!! -----------------------------------
-- | Makes assumption that typeOf not use it's only parameter any way because of 'undefined'.
typeOfFunParams :: Parameters a => Function a b -> [TypeRepr]
typeOfFunParams    = paramList . unliftArg
    where unliftArg :: Function a b -> a
          unliftArg = error "unliftArg"
          
typeOfFunRet :: RetType b => Function a b -> TypeRepr
typeOfFunRet       = typeOf . unliftRet
  where unliftRet  :: Function a b -> b
        unliftRet  = error "unliftRet"
        
mkParList :: Parameters a => Function a b -> ParList
mkParList f        = ParList $ zipWith Parameter (typeOfFunParams f) parNames
  where parNames   = map (mkVarName variablePrefix) [1..]

mkSignature :: (Parameters a, RetType b) => Function a b -> Signature
mkSignature f@(Fun domen) = 
  case domen of 
    Embedded name funs _    -> Signature retT name (mkParList f) funs
    BuiltIn (Ordinary name) -> Signature retT name (mkParList f) $ Funargs []
    BuiltIn (BOP name )     -> Signature retT (escape name) (mkParList f) $ Funargs []
    BuiltIn (UOP name )     -> Signature retT (escape name) (mkParList f) $ Funargs []
    _                       -> error "mkSignature"
  where retT = typeOfFunRet f
        escape :: String -> String
        escape = ("esc" ++) . concatMap (show . ord)

data StripError = IncorrectID   Identifier
                | BuiltInSymbol Spelling
                | UnknownError  String
                deriving Show

instance Error StripError where
  noMsg  = UnknownError "unknown error"
  strMsg = UnknownError

variablePrefix :: String
variablePrefix = "v"

checkSymbol :: Identifier -> Bool
checkSymbol = const True

-- TODO: split stripping in two stages
class StripLocal a b where
  strip :: a -> DDF TLDecl b
  
class StripGlobal a where
  stripG :: a -> Either StripError (DDN TLDecl)

instance StripLocal (Expression a) Expr where
  strip (Lit a)       = return $ Literal  $ showLit a
  strip (Var a)       = return $ Variable $ mkVarName variablePrefix a
  strip (App fun@(Fun domain) arg) = do case  stripG fun of
                                          Left  _ -> return () -- holdBuiltInSymbol a
                                          Right a -> dependsOn a
                                        handleStyle domain <$> strip arg
  
  -- UNSAFE!!! But it's impossible to construct unary op with two pars or ..e.t.c
    where handleStyle :: Domain a -> [Expr] -> Expr
          handleStyle (Embedded _       _  _)           e = Funcall (mkSignature fun) e
          handleStyle (BuiltIn (Ordinary _))            e = Funcall (mkSignature fun) e
          handleStyle (BuiltIn (UOP      name)) (a:_)     = UnaryOp  name a
          handleStyle (BuiltIn (BOP      name)) (a:b:_)   = BinaryOp name a b
          handleStyle (BuiltIn (TOP          )) (a:b:c:_) = TernaryOp a b c
          handleStyle _ _                                 = error "handleStyle"

--          holdBuiltInSymbol :: MonadError StripError m => StripError -> m ()
--          holdBuiltInSymbol = const $ return ()
--          holdBuiltInSymbol (BuiltInSymbol _) = return ()
--          holdBuiltInSymbol a                 = throwError a

instance Parameters a => StripLocal a [Expr] where
  strip = list

instance StripLocal a b => StripLocal (Maybe a) (Maybe b) where
  strip a = maybe (return Nothing) (fmap Just . strip) a

instance StripLocal (Claim r) Statement where
  strip (Ret e)      = Return <$> strip e
  strip (Def i e)    = Definition (typeOfExpr e) (mkVarName variablePrefix i) . Just <$> strip e
  strip (While c b)  = WhileLoop <$> strip c <*> strip b
  strip (Assign p q) = Assignment <$> strip p <*> strip q

-- | /()/ in second type variable in /Block/ ensures that we do not have orphaned expressions
instance StripLocal (Body r) CompoundStatement where
  strip body = CompoundStatement <$> mapM strip (context body)

instance StripGlobal (Function a b) where
  stripG (Fun (BuiltIn spelling))           = throwError $ BuiltInSymbol spelling
  stripG f@(Fun (Embedded identif funargs body)) = do
                        unless (checkSymbol identif) $
                          throwError $ IncorrectID identif
                        return $ mkNode (ProcDef <$> Procedure (mkSignature f) <$> strip body)


instance Parameters a => StripGlobal (a -> Expression b) where
  stripG = stripG . unsafeUnApp
