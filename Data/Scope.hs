-- | This module defines 'Scope' monad and related helpers.
--   It can be used as underlying monad:
-- 
--     * Module scope;
--
--     * 'Block' scope, such as compound statements in C;
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Scope  ( 
       -- * Scope monad
         Scope, Index
       -- * Manipulating scope context
       , add, freshIndex
       -- * Names
       , mkVarName, initialIndex
       -- * Unlifting scope context
       , context
       -- * Markers
       , Begin, End, begin, end
       ) where

import Control.Monad.State (StateT, runStateT, modify, get)
import Control.Monad.Writer (Writer, runWriter, tell)

-- | Int is sufficient for variable indexes because it's impossible to compile so big program.
--   We don't have so much videomemory assigned for code.
type Index = Int

-- | 'Scope' monad is convenient monad transformer wrapper 
newtype Scope r a = Scope { runScope :: StateT Index (Writer [r]) a }
                    deriving (Functor, Monad)

-- | The /initialIndex/ in index assigned for first entity in block.
initialIndex :: Index
initialIndex = 0

-- | 'freshIndex' should give unique index each time we call it within the /given/ scope.
freshIndex :: Scope r Index
freshIndex = Scope $ modify succ >> get

-- | Makes a name for a given 'Index'. Note that unique names are guarantted when prefix is fixed for all items in block.
mkVarName :: String -> Index -> String
mkVarName prefix index = prefix ++ show index

-- | 'add' pushes argument /to the end/ of the scope.
add :: r -> Scope r ()
add s = Scope $ tell [s]

-- | 'context' returns an accumulated context.
context :: Scope r a -> [r]
context = snd . runWriter . (`runStateT` initialIndex) . runScope

-- | 'Begin' datatype have one constructor which not supposed to be used somehow. 
--   The constructor defined to escape use of EmptyDataDecls extension and 'undefined'.
data Begin = Begin
-- | The same as 'Begin'.
data End = End

-- | 'begin' is special marker which might sign /beginning/ of the scope.
begin :: Scope r Begin
begin = return Begin

-- | 'end' is special marker which might sign /ending/ of the scope.
end :: Scope r End
end = return End
