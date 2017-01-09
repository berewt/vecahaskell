-- module for Trees
-- (c) 2016 Pascal Poizat
-- github: @pascalpoizat
--
-- This module is useless if Data.Tree supports different types in nodes and in leaves.
-- (this module is also used to learn Haskell by the way)
module Tree ( Tree(..)
            , isValid
            , bimap
            , leaves
            , depth)
where

import           Control.Lens
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (catMaybes)
import           Data.Set           (Set, fromList)

-- tree with values of type a in the leaves and values of type b in the nodes
data Tree a b
  = Leaf a
  | Node b [Tree a b]
  deriving (Show,Eq)

-- extract the leaf value, if the given Tree is a Leaf
leafValue :: Tree a b -> Maybe a
leafValue (Leaf x) = Just x
leafValue (Node _ ts) = Nothing

-- extract the node value, if the given Tree is a Node
nodeValue :: Tree a b -> Maybe b
nodeValue (Leaf _) = Nothing
nodeValue (Node x _) = Just x

-- apply transformation f to the leaves of the tree and transformation g to its nodes
-- bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
instance Bifunctor Tree where
  bimap f g (Leaf x)    = Leaf (f x)
  bimap f g (Node x ts) = Node (g x) (fmap (bimap f g) ts)

-- The plate instance add neat functionalities for tree manipulation
instance Plated (Tree a b) where
  plate _ (Leaf x) = pure $ Leaf x
  plate f (Node x ts) = Node x <$> (traverse f ts)

-- checks for the validity of a tree
isValid :: Tree a b -> Bool
isValid (Leaf _)    = True
isValid (Node _ ts) = not (null ts)

-- get the set of all leaves
leaves :: Ord a => Tree a b -> Set a
leaves = fromList . catMaybes . fmap leafValue . universe

-- get the depth of the tree
depth :: Tree a b -> Int
depth = para (const $ (+1) . maximum . (0:))
