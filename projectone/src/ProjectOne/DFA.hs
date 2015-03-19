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
, newMove
, newMoves
) where

import qualified Data.Set as S
import ProjectOne.NFA

newtype DFA a = DFA (NFA a)

--newStep :: NFA Int -> S.Set Char -> DFA (S.Set Int) -> DFA (S.Set Int)
--newStep m@(NFA s e i a) al d@(DFA (NFA s' e' i' a')) =
--  DFA (NFA (S.fromList $ helper (DFA m) (S.toList al) d (S.toList s')))
--  where
--    helper _ _ d [] = d
--    helper m a d (x:xs) = helper m a (newMoves m x a d) xs

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
