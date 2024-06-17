{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
module Trie
  ( Trie
  , empty
  , insert
  , alter
  , unfoldTrie
  , fromList
  , matchLongestPrefix
  )
  where

import Control.Monad (foldM)
import qualified Data.IntMap as M
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

data Trie a = Trie (Maybe a) (Maybe (M.IntMap (Trie a)))
  deriving (Show, Eq, Ord)

instance Semigroup (Trie a) where
   trie1 <> trie2 = foldr (uncurry insert) trie1 (unfoldTrie trie2)

instance Monoid (Trie a) where
   mempty = Trie Nothing Nothing
   mappend = (<>)

empty :: Trie a
empty = Trie Nothing Nothing

unfoldTrie :: Trie a -> [([Int], a)]
unfoldTrie  = map (first reverse) . go []
 where
  go xs (Trie (Just v) (Just m)) =
    (xs, v) : concatMap (gopair xs) (M.toList m)
  go xs (Trie (Just v) Nothing) = [(xs, v)]
  go xs (Trie Nothing (Just m)) =
    concatMap (gopair xs) (M.toList m)
  go _ (Trie Nothing Nothing) = []
  gopair xs (i, trie) = go (i:xs) trie

insert :: [Int] -> a -> Trie a -> Trie a
insert [] x (Trie _ mbm) = Trie (Just x) mbm
insert (c:cs) x (Trie mbv (Just m)) =
  case M.lookup c m of
    Nothing   -> Trie mbv (Just (M.insert c (insert cs x empty) m))
    Just trie -> Trie mbv (Just (M.insert c (insert cs x trie) m))
insert (c:cs) x (Trie mbv Nothing) =
  Trie mbv (Just (M.insert c (insert cs x empty) mempty))

fromList :: [([Int], a)] -> Trie a
fromList = foldr (uncurry insert) mempty

alter :: (Maybe a -> Maybe a) -> [Int] -> Trie a -> Trie a
alter f [] (Trie mbv mbm) = Trie (f mbv) mbm
alter f (c:cs) (Trie mbv (Just m)) =
  Trie mbv (Just (M.insert c (alter f cs $ fromMaybe empty $ M.lookup c m) m))
alter f (c:cs) (Trie mbv Nothing) =
  Trie mbv (Just (M.insert c (alter f cs empty) mempty))

type MatchState a = (Maybe (a, Int, Trie a), Int, Trie a)
  -- best match so far, number of code points consumed, current subtrie

{-# SPECIALIZE matchLongestPrefix :: Trie a -> [Int] -> Maybe (a, Int, Trie a) #-}
-- returns Nothing for no match, or:
-- Just (value, number of code points consumed, subtrie)
matchLongestPrefix :: Foldable t => Trie a -> t Int -> Maybe (a, Int, Trie a)
matchLongestPrefix trie = either id getBest . foldM go (Nothing, 0, trie)
 where
   getBest (x,_,_) = x
   -- Left means we've failed, Right means we're still pursuing a match
   go :: MatchState a -> Int -> Either (Maybe (a, Int, Trie a)) (MatchState a)
   go (best, consumed, Trie _ mbm) c =
     case mbm >>= M.lookup c of
       -- char not matched: stop processing, return best so far:
       Nothing -> Left best
       -- char matched, with value: replace best, keep going:
       Just subtrie@(Trie (Just x) _)
               -> Right (Just (x, consumed + 1, subtrie), consumed + 1, subtrie)
       -- char matched, but not value: keep best, keep going:
       Just subtrie@(Trie Nothing _)
               -> Right (best, consumed + 1, subtrie)
