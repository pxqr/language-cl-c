-- | This module have several aims
-- 
--     * extract dependencies in convenient declarative way.
-- 
--     * build dependency graph from possible infinite data dependency tree.
-- 
--     * analize dependencies (inlining, linear recursion, mutial recursion)
-- 
--   Since dependency forest or tree might be infinitely large, usuall deep comparing won't work for finite time. Ord and Eq classes should not doing deep comparison, but instead some kind of shallow comparison. e.g. compare IDs
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Data.Dependencies   
       (-- * DDF
         DDF, dependsOn
        -- * DDN
       , DDN, mkNode, node
       , DDG, entries, depG, prune
        -- * Linking
       , resolveSD, Recursion, showError
       ) where

import Control.Arrow          (second) 
import Control.Applicative    (Applicative)
import Control.Monad.Writer   

import Data.Array    (Array, listArray, assocs, (!))
import Data.Graph    (Graph, Table, Vertex, topSort, scc)
import Data.Tree     (Tree(..), Forest, rootLabel, flatten)
import Data.Map as M (Map, insert, member, empty, keys, lookup, fromList) 
import Data.Set as S (Set, elems, fromList, toList, singleton) 

import Data.Maybe    (mapMaybe, fromMaybe)
import Data.Function (on)
import Data.List     (intercalate)
import Data.Ord      (comparing)
import Data.Tuple    (swap)


-- | Key function in the module which transform infinity 'Tree' to 'Map' of 'Set's. It assumes that:
--     * forall (Node a as) (Node b bs). a == b => as == bs
--   where == is shallow equality. e.g. of (==): 
--     * equality of identifiers 
--     * equality of term but excluding all dependencies (since we can have recursive dependencies 
--          which leads to infitity forest which lead to infinity loop)
collectTree :: Ord s => Map s [s] -> Tree s -> Map s [s]
collectTree dm (Node root subF) 
  | root `M.member` dm = dm
  |     otherwise    = M.insert root (fmap rootLabel subF) dm `collectForest` subF
                                             
-- | /collectForest/ is auxillary function for 'collectTree'. 
--   It assumes the same properties as 'collectTree' but for 'Forest'.
collectForest :: Ord s => Map s [s] -> Forest s -> Map s [s]
collectForest = foldl collectTree

-- | /DDN/ stands for Data Dependency Node. 
--   This wrapper is needed because we gonna put it in map. So if we
newtype DDN s = DDN { node :: Tree s }

instance Eq s => Eq (DDN s) where
  (==) = (==) `on` (rootLabel . node)
  
instance Ord s => Ord (DDN s) where
  compare = comparing (rootLabel . node)

-- | /DDF/ stands for Data Dependency Forest.
--   Note that strict 'Set' can break it all, because /DDN/s in 'State' should be unevaluated.
newtype DDF s a = DDF { runDDF :: Writer (Set (DDN s)) a }
                  deriving (Functor, Applicative, Monad)

-- | strict in Ord methods
dependsOn :: Ord s => DDN s -> DDF s ()
dependsOn = DDF . tell . S.singleton

-- | /ddn/ makes a node which will be tracked as a single point of dependency. keeping result in monad allows to make several points.
mkNode :: DDF s s -> DDN s
mkNode = DDN . uncurry Node . second (map node . S.elems) . runWriter . runDDF

-- | /DDG/ stands for Data Dependency Graph.
data DDG s = DDG { entries :: Table s -- mapping from vertexes to entities
                 , depG    :: Graph   -- dependency graph itself
                 }

-- | Prune Tree so it is becomes a finite 'Map'. Then convert the 'Map' to convenient data graph.
prune :: Ord s => DDN s -> DDG s 
prune = mkDDG . collectTree M.empty . node

mkDDG :: Ord s => Map s [s] -> DDG s
mkDDG m = DDG ents graph
 where graph   = fmap (mapMaybe (`M.lookup` vertmap) . nubS . fromMaybe [] . (`M.lookup` m)) ents
       vertmap = entToVert ents
       ents = mkTable $ M.keys m

       mkTable :: [a] -> Table a
       mkTable xs = listArray (1, length xs) xs
       
       entToVert :: Ord s => Array Vertex s -> Map s Vertex
       entToVert = M.fromList . fmap swap . assocs
       
       nubS :: Ord a => [a] -> [a]
       nubS = S.toList . S.fromList

----------------------- dependency analysis -----------------------
data Recursion a = Auto a | Mutial [a]
                 deriving (Show, Functor)

-- | Gives all recursive dependencies in a given graph.
recursiveDeps :: DDG a -> [Recursion a]
recursiveDeps (DDG es dg) = map (fmap (es !)) . mapMaybe (mkRec . flatten) $ scc dg
  where mkRec :: [Vertex] -> Maybe (Recursion Vertex)
        mkRec [a] | a `elem` dg ! a = Just $ Auto a
                  | otherwise               = Nothing
        mkRec as  = Just $ Mutial as

---------------------- linking              -----------------------
-- | List in topological order of each elemens of a given graph.
order :: DDG a -> [a]
order (DDG es dg) = reverse $ map (es !) $ topSort dg

-- | Resolve symbol dependencies. 
--   
--     * If there are recursive dependencies then returned an error --- a
--
--     * If there are no rec deps then each element of graph returned in top order.
--
resolveSD :: Ord a => DDG a -> Either [Recursion a] [a]
resolveSD g = do let loops = recursiveDeps g
                 unless (null loops) $ 
                   Left loops
                 return $ order g

-- | Prettifyed error formatting.
showError :: (a -> String) -> Recursion a -> String
showError identifier (Auto a)    = "recursive symbol: " ++ identifier a
showError identifier (Mutial as) = "cyclic recursion: " ++ (intercalate " " $ fmap identifier as)
