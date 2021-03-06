{-|
Module      : Models.LabelledTransitionSystem
Description : A type for Labelled Transition Systems (LTS).
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Models.LabelledTransitionSystem (
    -- * constructors
    State(..)
  , Transition(..)
  , LabelledTransitionSystem(..)
  , LTS
  , Path(..)
  , ComputationTree
    -- * validity checking
  , isValidLTS
    -- * reachability
  , successors
  , predecessors
  , xreachables
  , reachables
  , coreachables
    -- * paths and traces
  , (<>)
  , trace
  , start
  , end
  , pathStates
  , pathStatesUnique
  , paths
  , paths'
  , pathsFrom
  , pathStartsWith
  , pathEndsWith
  , treePaths
    -- * properties
  , hasLoop
  , isSelfReachable
    -- * model to model transformations
  , toComputationTree
  , Models.LabelledTransitionSystem.toDot)
where

import           Control.Monad (join)
import           Data.GraphViz (DotGraph, graphElemsToDot, nonClusteredParams)
import           Data.Maybe   (listToMaybe)
import           Data.Monoid   (Any (..), (<>))
import           Data.Set      (fromList)
import           Helpers       (allIn, fixpoint', removeDuplicates)
import           Trees.Tree

{-|
A state over some type a.
-}
newtype State a =
  State a
  deriving (Eq, Ord)

{-|
Show instance for states.
-}
instance (Show a) => Show (State a) where
  show (State a) = show a

{-|
A transition with a label of type a.
-}
data Transition a b = Transition
  { source :: State b -- ^ source state of the 'Transition'
  , label  :: a -- ^ label of the 'Transition'
  , target :: State b -- ^ target state of the 'Transition'
  } deriving (Eq, Ord)

{-|
Show instance for transitions.
-}
instance (Show a, Show b) => Show (Transition a b) where
  show (Transition s l s') = unwords [show s, "-|", show l, "|->", show s']

{-|
A Labelled Transition System (LTS) with labels of type a.
-}
data LabelledTransitionSystem a b = LabelledTransitionSystem
  { alphabet     :: [a] -- ^ alphabet
  , states       :: [State b] -- ^ set of states
  , initialState :: State b -- ^ initial state
  , finalStates  :: [State b] -- ^ set of final states
  , transitions  :: [Transition a b] -- ^ set of transitions
  } deriving (Show)

{-|
Eq instance for LTSs.

Eq is up to reordering in collections.
-}
instance (Ord a, Ord b) => Eq (LabelledTransitionSystem a b) where
  (LabelledTransitionSystem as ss s0 fs ts) == (LabelledTransitionSystem as' ss' s0' fs' ts') =
    fromList as == fromList as' &&
    fromList ss == fromList ss' &&
    s0 == s0' &&
    fromList fs == fromList fs' &&
    fromList ts == fromList ts'

{-|
Alias for LTS.
-}
type LTS = LabelledTransitionSystem

{-|
Check the validity of an LTS.

An LTS is valid iff:
- the alphabet is not empty
- the set of states is not empty
- the initial state is in the set of states
- each final state is in the set of states
- the source state of each transition is in the set of states
- the label of each transition is in the alphabet
- the target state of each transition is in the set of states
-}
isValidLTS :: (Ord a, Ord b) => LTS a b -> Bool
isValidLTS (LabelledTransitionSystem as ss s0 fs ts)
  | null as = False
  | null ss = False
  | s0 `notElem` ss = False
  | not $ fs `allIn` ss = False
  | not $ (source <$> ts) `allIn` ss = False
  | not $ (label <$> ts) `allIn` as = False
  | not $ (target <$> ts) `allIn` ss = False
  | otherwise = True

{-|
Check if there are loops in an LTS.
-}
hasLoop :: (Ord b) => LabelledTransitionSystem a b -> Bool
hasLoop (LabelledTransitionSystem _ ss _ _ ts) =
  getAny $ foldMap (Any . isSelfReachable ts) ss

{-|
Check if a state is reachable from itself.
-}
isSelfReachable :: (Ord b) => [Transition a b] -> State b -> Bool
isSelfReachable ts s = s `elem` reachables ts s

{-|
Get the states reachable from a state in one step.
-}
successors :: (Ord b) => [Transition a b] -> State b -> [State b]
successors ts s = target <$> filter ((== s) . source) ts

{-|
Get the states co-reachable from a state in one step.
-}
predecessors :: (Ord b) => [Transition a b] -> State b -> [State b]
predecessors ts s = source <$> filter ((== s) . target) ts

{-|
Get all states f-reachable from a state, where f is a step function.
-}
xreachables ::
     (Ord b)
  => ([Transition a b] -> State b -> [State b])
  -> [Transition a b]
  -> State b
  -> [State b]
xreachables f ts s = fixpoint' (step f ts) $ f ts s
  where
    step f' ts' ss = ss <> foldMap (f' ts') ss

{-|
Get all states reachable from a state.
-}
reachables :: (Ord b) => [Transition a b] -> State b -> [State b]
reachables = xreachables successors

{-|
Get all states co-reachable from a state.
-}
coreachables :: (Ord b) => [Transition a b] -> State b -> [State b]
coreachables = xreachables predecessors

{-|
A path is a list of transitions (si,li,s'i).

We do not verify the property that for each i we have si+1=s'i.
-}
newtype Path a b =
  Path [Transition a b]
  deriving (Eq, Ord, Show)

{-|
Monoid instance for paths.
-}
instance Monoid (Path a b) where
  mempty = Path []
  mappend (Path xs) (Path xs') = Path (xs <> xs')

{-|
A computation tree for an LTS.
-}
type ComputationTree a b = Tree (State b) (State b) a

{-|
Get the start of a path.
-}
start :: Path a b -> Maybe (Transition a b)
start (Path ts) = listToMaybe ts

{-|
Get the end of a path.
-}
end :: Path a b -> Maybe (Transition a b)
end (Path []) = Nothing
end (Path ts) = Just (last ts)

{-|
Get the trace of a path.
-}
trace :: Path a b -> [a]
trace (Path ts) = label <$> ts

{-|
Get all states in a path (ordered, possible duplicates).

A path s0->s1->s2->s0->s3 yields [s0,s1,s2,s0,s3].
-}
pathStates :: Path a b -> [State b]
pathStates (Path [])     = []
pathStates (Path (t:ts)) = source t : target t : (target <$> ts)

{-|
Get all states in a path (non ordered, no duplicates).

A path s0->s1->s2->s0->s3 may yield any possible list taken from the set {s0,s1,s2,s3}
i.e., possibly [s3,s2,s1,s0], not sure to be [s0,s1,s2,s3].
-}
pathStatesUnique :: Ord b => Path a b -> [State b]
pathStatesUnique (Path ts) =
  removeDuplicates $ foldMap (\(Transition s _ s') -> [s, s']) ts

{-|
Get all paths from all states.

Can be infinite.
-}
paths' :: (Ord a, Ord b) => LabelledTransitionSystem a b -> [Path a b]
paths' l = foldMap (`pathsFrom` l) (states l)

{-|
Get all paths from the initial state.

Can be infinite.
-}
paths :: (Ord a, Ord b) => LabelledTransitionSystem a b -> [Path a b]
paths l = pathsFrom (initialState l) l

{-|
Get all paths from some state.

Can be infinite.
-}
pathsFrom ::
     (Ord a, Ord b) => State b -> LabelledTransitionSystem a b -> [Path a b]
pathsFrom s l = treePaths $ toComputationTree s l

{-|
Check if a path begins with a transition that satifies some property.

Yields false if the path is empty.
-}
pathStartsWith :: (Transition b a -> Bool) -> Path b a -> Bool
pathStartsWith f (Path ts) = maybe False f $ listToMaybe ts

{-|
Check if a path ends with a transition that satifies some property.

Does not work if the path is infinite.
-}
pathEndsWith :: (Transition b a -> Bool) -> Path b a -> Bool
pathEndsWith f (Path ts) = f $ last ts

{-|
Get paths in a computation tree.

Can be infinite.
-}
treePaths :: (Ord a, Ord b) => ComputationTree a b -> [Path a b]
treePaths = f mempty
  where
    f p (Leaf _)    = [p]
    f p (Node s ts) = p : foldMap (g p s) ts
    g p s (a, t'@(Leaf s'))   = f (p <> Path [Transition s a s']) t'
    g p s (a, t'@(Node s' _)) = f (p <> Path [Transition s a s']) t'

{-|
Build a computation tree from an LTS (starting with a distinct state).

Can be infinite.
-}
toComputationTree ::
     (Eq b) => State b -> LabelledTransitionSystem a b -> ComputationTree a b
toComputationTree s l@(LabelledTransitionSystem _ _ _ _ ts)
  | null ts' = Leaf s
  | otherwise = Node s $ foldMap f ts'
  where
    ts' = filter ((== s) . source) ts
    f (Transition _ x s2) = [(x, toComputationTree s2 l)]

{-|
Model to text transformation from LTS to dot.
-}
toDot :: (Ord a, Ord b) => LabelledTransitionSystem a b -> DotGraph (State b)
toDot (LabelledTransitionSystem _ ss _ _ ts) =
  graphElemsToDot
    nonClusteredParams
    (stateToDotState <$> ss)
    (transitionToDotEdge <$> ts)

{-|
Model to text transformation from a state to dot.

Helper for the LTS to dot transformation.
-}
stateToDotState :: State a -> (State a, State a)
stateToDotState = join (,)

{-|
Model to text transformation from a transtion to dot.

Helper for the LTS to dot transformation
-}
transitionToDotEdge :: Transition a b -> (State b, State b, a)
transitionToDotEdge t = (source t, target t, label t)
