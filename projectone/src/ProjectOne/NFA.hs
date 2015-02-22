-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.NFA
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- This module exports functions and types for working with 'NFA's.
----------------------------------------------------------------------------
module ProjectOne.NFA (
  NFA (..)
, Edge (..)
, fromRegex
) where

import qualified Data.Set as S
import qualified ProjectOne.RegexRule as R

-- | Here we represent a nondeterministic finite automata (NFA).
data NFA a = NFA { states    :: S.Set a
                 , edges     :: S.Set (Edge a)
                 , initial   :: a
                 , accepting :: S.Set a
                 } deriving (Eq, Ord, Show)

-- | This type defines edges that appear in our graphs. An edge can either be a
-- character or ε (the empty string). Right now we are using the literal 'Char'
-- type that Haskell gives us. In reality, we should probably generalize this
-- more, but that isn't necessary to complete the assignment.
data Edge a = Edge a Char a
            | Epsilon a a
            deriving (Eq, Ord, Show)

alterEdge :: Int -> Edge Int -> Edge Int
alterEdge n (Edge x c y) = Edge (n + x) c (n + y)
alterEdge n (Epsilon x y) = Epsilon (n + x) (n + y)

fromRegex :: R.RegexRule -> NFA Int
fromRegex (R.Literal c) = NFA
                          (S.fromList [0, 1])
                          (S.singleton $ Edge 0 c 1)
                          0
                          (S.singleton 1)
fromRegex R.Epsilon = NFA
                      (S.fromList [0, 1])
                      (S.singleton $ Epsilon 0 1)
                      0
                      (S.singleton 1)
fromRegex (R.Star r) = f . fromRegex $ r
  where
    f (NFA s e _ _) = NFA
                      (S.map (+1) s `S.union` S.fromList [0, S.size s + 1])
                      (S.map (alterEdge 1) e `S.union` S.fromList [ Epsilon 0 1
                                                                  , Epsilon (S.size s) 1
                                                                  , Epsilon 0 (S.size s + 1)
                                                                  , Epsilon (S.size s) (S.size s + 1)
                                                                  ])
                      0
                      (S.singleton (S.size s + 1))
fromRegex (R.Concat r1 r2) =
  f (fromRegex r1) (fromRegex r2)
  where
    f (NFA s1 e1 i1 _) (NFA s2 e2 _ a2) =
      NFA
      (s1 `S.union` S.map (+ (S.size s1 - 1)) s2)
      (e1 `S.union` S.map (alterEdge (S.size s1 - 1)) e2)
      i1
      (S.map (+ (S.size s1 - 1)) a2)
