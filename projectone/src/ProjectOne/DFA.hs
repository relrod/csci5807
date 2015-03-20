-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.DFA
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Functions for converting from an 'NFA' to a 'DFA'.
----------------------------------------------------------------------------
module ProjectOne.DFA (
  DFA (..)
, newStep
, newMoves
, newMove
) where

import qualified Data.Set as S
import ProjectOne.NFA

newtype DFA a = DFA (NFA a)

newStep :: NFA Int -> String -> DFA (S.Set Int) -> DFA (S.Set Int)
newStep m al d@(DFA (NFA s' _ _ _)) =
  helper (DFA m) al d (S.toList s')
  where
    helper _ _ d' [] = d'
    helper m' al' d' (x:xs) = helper m' al' (newMoves m' x al' d') xs

-- TODO: This should be a fold or something instead
newMoves :: DFA Int -> S.Set Int -> String -> DFA (S.Set Int) -> DFA (S.Set Int)
newMoves _ _ [] d = d
newMoves d@(DFA m) s (x:xs) d' = newMoves d s xs (newMove m s x d')

newMove :: NFA Int -> S.Set Int -> Char -> DFA (S.Set Int) -> DFA (S.Set Int)
newMove m@(NFA _ _ _ a) x c (DFA (NFA s' e' i' a')) =
  let newTrans = singleTransition m c x
  in DFA $ NFA
     (s' `S.union` S.singleton newTrans)
     (e' `S.union` S.singleton (Edge x c newTrans))
     i'
     (if a `S.intersection` newTrans == S.empty
      then a'
      else a' `S.union` S.singleton newTrans)
