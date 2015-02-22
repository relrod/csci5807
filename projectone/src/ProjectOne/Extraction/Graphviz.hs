-----------------------------------------------------------------------------
-- |
-- Module : ProjectOne.Extraction.Graphviz
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Extracts 'NFA' and 'DFA' edges and nodes into Graphviz graphs.
----------------------------------------------------------------------------
module ProjectOne.Extraction.Graphviz where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (Epsilon)
import Data.Set hiding (map)
import ProjectOne.NFA

nfaNodes :: NFA a -> [(a, String)]
nfaNodes (NFA s _ _ _) = map (\x -> (x, "")) (toList s)

nfaEdges :: NFA a -> [(a, a, String)]
nfaEdges (NFA _ e _ _) = map toEdge (toList e)
  where
    toEdge (Epsilon a b) = (a, b, "ε")
    toEdge (Edge a ch b) = (a, b, [ch])

outputNfaGraph :: NFA Int -> IO FilePath
outputNfaGraph nfa = addExtension (runGraphviz (nfaGraph nfa)) Png "graph"

nfaGraph :: NFA Int -> DotGraph Int
nfaGraph nfa = graphElemsToDot graphParams (nfaNodes nfa) (nfaEdges nfa)

graphParams :: GraphvizParams Int String String () String
graphParams = defaultParams { fmtEdge = \(_, _, el) -> [toLabel el]
                            , globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
                            }
