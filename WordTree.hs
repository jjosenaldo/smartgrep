module WordTree where
import Prelude hiding (Word)
import Map ( Map, newMap, emptyMap, set, get, entries, isMapEmpty )
import Word
    ( Word,
      PrefixResult(prPrefix, prRemainder2, prRemainder1),
      commonPrefix,
      isEmpty, first, concatWord, emptyWord, wordToArray )
import Data.Array (Array)
import WordOccurrences (WordOccurrences (WordOccurrences, linesTimes), isOccurrencesEmpty, addOccurrences)

type WTEdge = (Word, WTNode)

newtype WT = WT {roots :: Map Char WTEdge}

data WTNode = WTNode {
  word :: Array Int Char,
  children :: Map Char WTEdge,
  occurrences :: WordOccurrences
}

instance Show WT where
  show (WT roots) = concatMap (\(_, (word, node)) -> "--" ++ word ++ show node) (entries roots)

instance Show WTNode where
  show WTNode {children = children} =
    "-->(" ++ concatMap (\(_, (word, node)) -> "--" ++ word ++ show node) (entries children) ++ ")"

emptyTree :: WT
emptyTree = WT emptyMap

emptyOccurrences :: WordOccurrences
emptyOccurrences = WordOccurrences { linesTimes = emptyMap }

isFinal :: WTNode -> Bool 
isFinal WTNode {occurrences = occs} = not $ isOccurrencesEmpty occs

insert :: WT -> Word -> WordOccurrences -> WT
insert (WT roots) word occurrences =
  case maybeExistingChild of
      Nothing -> WT $ set roots (first word) (word, wordAsNewNode)
      Just (childWord, child) -> WT $ set roots (first word) $ insertAtEdge child childWord emptyWord word occurrences
    where
      maybeExistingChild = get roots (first word)
      wordAsNewNode = WTNode {word = wordToArray word, children = emptyMap, occurrences = occurrences}


-- (assumes that the suffix shares a common prefix with the edge)
insertAtEdge :: WTNode -- node in which we'll insert the word
               -> Word -- edge word
               -> Word -- current prefix
               -> Word -- current suffix
               -> WordOccurrences -- occurrences to insert 
               -> WTEdge -- the new edge and the new node after the insertion
insertAtEdge child edgeWord currPrefix currSuffix newOccurrences
  -- at <-- at
  | isEmpty remainderSuffix && isEmpty remainderEdge = (edgeWord, child {occurrences =  addOccurrences (occurrences child) newOccurrences})
  -- at <-- a: a (t)
  | isEmpty remainderSuffix = (currSuffix, WTNode {
      word = wordToArray wordBeingInserted,
      occurrences = newOccurrences,
      children = newMap (first remainderEdge) (remainderEdge, child)
    })
  -- at <-- ab: a (t, b)
  | not $ isEmpty remainderEdge = (prefix, WTNode {
    word = wordToArray $ concatWord currPrefix prefix,
    occurrences = emptyOccurrences,
    children = set (newMap (first remainderSuffix) (remainderSuffix, WTNode{word = wordToArray wordBeingInserted, children = emptyMap, occurrences = newOccurrences}))
      (first remainderEdge) (remainderEdge, child)
  })
  -- at <-- ate: at (e)
  | otherwise = case maybeGrandchildSamePrefix of
      -- at (ev, lol) <-- ate: at (e (v), lol)
      Just grandchildSamePrefix -> (prefix, child {children = set (children child) (first remainderSuffix) newNodeWithEdge})
        where newNodeWithEdge = insertAtEdge grandchild edgeGrandchild (concatWord currPrefix prefix) remainderSuffix newOccurrences
              grandchild = snd grandchildSamePrefix
              edgeGrandchild = fst grandchildSamePrefix

      -- at (lol) <-- ate: at (e, lol)
      Nothing -> (prefix, child {children = set (children child) (first remainderSuffix) (remainderSuffix, newNode) })
        where newNode = WTNode{word = wordToArray wordBeingInserted, occurrences = newOccurrences, children = emptyMap}
  where
    prefixResult = commonPrefix edgeWord currSuffix
    prefix = prPrefix prefixResult
    remainderSuffix = prRemainder2 prefixResult
    remainderEdge = prRemainder1 prefixResult
    maybeGrandchildSamePrefix = get (children child) (first remainderSuffix)
    wordBeingInserted = concatWord currPrefix currSuffix
