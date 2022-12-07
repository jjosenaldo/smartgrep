module WordTree where
import Prelude hiding (Word)
import Map ( Map, newMap, emptyMap, set, get, entries )
import Word
    ( Word,
      PrefixResult(prPrefix, prRemainder2, prRemainder1),
      commonPrefix,
      isEmpty, first, concatWord, emptyWord, wordToArray )
import Data.Array (Array)

newtype WordOccurrence = WordOccurrence {line :: Int} deriving (Show)

type WTEdge = (Word, WTNode)

newtype WT = WT {roots :: Map Char WTEdge}

data WTNode = WTNode {
  word :: Array Int Char,
  children :: Map Char WTEdge,
  occurrences :: [WordOccurrence]
}

instance Show WT where
  show (WT roots) = concatMap (\(_, (word, node)) -> "--" ++ word ++ show node) (entries roots)

instance Show WTNode where
  show WTNode {children = children} =
    "-->(" ++ concatMap (\(_, (word, node)) -> "--" ++ word ++ show node) (entries children) ++ ")"

emptyTree :: WT
emptyTree = WT emptyMap

isFinal :: WTNode -> Bool 
isFinal WTNode {occurrences = occs} = null occs

insert :: WT -> Word -> WordOccurrence -> WT
insert (WT roots) word occurrence =
  case maybeExistingChild of
      Nothing -> WT $ set roots (first word) (word, wordAsNewNode)
      Just (childWord, child) -> WT $ set roots (first word) $ insertAtEdge child childWord emptyWord word occurrence
    where
      maybeExistingChild = get roots (first word)
      wordAsNewNode = WTNode {word = wordToArray word, children = emptyMap, occurrences = [occurrence]}


-- (assumes that the suffix shares a common prefix with the edge)
insertAtEdge :: WTNode -- node in which we'll insert the word
               -> Word -- edge word
               -> Word -- current prefix
               -> Word -- current suffix
               -> WordOccurrence -- occurrence to insert 
               -> WTEdge -- the new edge and the new node after the insertion
insertAtEdge child edgeWord currPrefix currSuffix occurrence
  -- at <-- at
  | isEmpty remainderSuffix && isEmpty remainderEdge = (edgeWord, child {occurrences = occurrence : occurrences child})
  -- at <-- a: a (t)
  | isEmpty remainderSuffix = (currSuffix, WTNode {
      word = wordToArray wordBeingInserted,
      occurrences = [occurrence],
      children = newMap (first remainderEdge) (remainderEdge, child)
    })
  -- at <-- ab: a (t, b)
  | not $ isEmpty remainderEdge = (prefix, WTNode {
    word = wordToArray $ concatWord currPrefix prefix,
    occurrences = [],
    children = set (newMap (first remainderSuffix) (remainderSuffix, WTNode{word = wordToArray wordBeingInserted, children = emptyMap, occurrences = [occurrence]}))
      (first remainderEdge) (remainderEdge, child)
  })
  -- at <-- ate: at (e)
  | otherwise = case maybeGrandchildSamePrefix of
      -- at (ev, lol) <-- ate: at (e (v), lol)
      Just grandchildSamePrefix -> (prefix, child {children = set (children child) (first remainderSuffix) newNodeWithEdge})
        where newNodeWithEdge = insertAtEdge grandchild edgeGrandchild (concatWord currPrefix prefix) remainderSuffix occurrence
              grandchild = snd grandchildSamePrefix
              edgeGrandchild = fst grandchildSamePrefix

      -- at (lol) <-- ate: at (e, lol)
      Nothing -> (prefix, child {children = set (children child) (first remainderSuffix) (remainderSuffix, newNode) })
        where newNode = WTNode{word = wordToArray wordBeingInserted, occurrences = [occurrence], children = emptyMap}
  where
    prefixResult = commonPrefix edgeWord currSuffix
    prefix = prPrefix prefixResult
    remainderSuffix = prRemainder2 prefixResult
    remainderEdge = prRemainder1 prefixResult
    maybeGrandchildSamePrefix = get (children child) (first remainderSuffix)
    wordBeingInserted = concatWord currPrefix currSuffix

t1 = insert emptyTree "test" (WordOccurrence{line=1})
t2 = insert t1 "toaster" (WordOccurrence{line=2})
t3 = insert t2 "toasting" (WordOccurrence{line=3})
t4 = insert t3 "slow" (WordOccurrence{line=4})
t5 = insert t4 "slowly" (WordOccurrence{line=5})

