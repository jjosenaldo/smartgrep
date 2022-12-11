module Search where
import Word (Word, isEmpty, PrefixResult (..), commonPrefix, first, charAt, size)
import Prelude hiding (Word)
import WordTree (WT (..), WTNode (..), WTEdge, isFinal, emptyOccurrences)
import Map (get, entries, values, Map)
import OrderedList (OrderedList, concatList, emptyList, insertAtList)
import Data.Array ( Array, bounds, (!), elems, listArray )
import Data.Array.Base (array)
import GHC.Stack (HasCallStack)
import WordOccurrences (WordOccurrences)

type MatrixLine = Array Int Int
type WordChars = Array Int Char

data SearchResult = SearchResult {
    sr_word :: Array Int Char,
    distance :: Int,
    sr_occurrences :: WordOccurrences
}

instance Show SearchResult where
  show SearchResult{sr_word=sr_word, distance=distance, sr_occurrences=sr_occurrences}= "{" ++ show distance ++ " " ++ elems sr_word ++ "}"

getClosestResults :: [SearchResult] -- all results
                  -> Int -- maxium distance
                  ->  [SearchResult] -- closest results
getClosestResults allResults maxDistance = takeWhile (\result -> distance result <= maxDistance) allResults

buildTextFromClosestResults :: Int 
                            -> Int 
                            -> [SearchResult] 
                            ->  String
buildTextFromClosestResults maxDistance maxSuggestions = buildTextFromResults maxSuggestions . flip getClosestResults maxDistance

buildTextFromResults :: Int -> [SearchResult] -> String
buildTextFromResults maxSuggestions results
        | null results = "No results found"
        | distance (head results) == 0 = show  (sr_occurrences $ head results) ++ closeResultsText (tail results)
        | otherwise = closeResultsText results
                where
                        closeResultsText results = concatMap (\result -> elems (sr_word result) ++ "\n") (take maxSuggestions results)


instance Eq SearchResult where
  res1 == res2 = distance res1 == distance res2

instance Ord SearchResult where
  res1 <= res2 = distance res1 <= distance res2

searchTree  :: WT
            -> Word -- word to search
            -> OrderedList SearchResult -- results
searchTree (WT roots) wordToSearch = searchNode (rootNode roots) 0 wordChars (firstMatrixLine wordChars)
  where wordChars = listArray (0,size wordToSearch - 1) wordToSearch

rootNode :: Map Char WTEdge -> WTNode
rootNode roots = WTNode (array (0,0) []) roots emptyOccurrences

searchNode :: WTNode  -- current node
           -> Int -- current index
           -> WordChars -- word to search
           -> MatrixLine -- prev matrix line
           -> OrderedList SearchResult -- new results
searchNode node index wordChars prevLine =
  concatList  childrenResults nodeResults
  where
    nodeResults = if not $ isFinal node
                  then emptyList
                  else insertAtList emptyList $ SearchResult{sr_occurrences=occurrences node,distance=distanceCurrentWord,sr_word=word node}
    wordBounds = bounds wordChars
    wordLength = snd wordBounds - fst wordBounds + 1
    distanceCurrentWord = prevLine ! wordLength
    currentOccurrences = occurrences node
    edges = values $ children node
    childrenResultsList = fmap (\edge@(edgeWord, edgeNode) -> searchNode edgeNode (index + size edgeWord) wordChars (resultsFromEdge edge)) edges
    childrenResults = foldr concatList emptyList childrenResultsList
    resultsFromEdge (edgeWord,_) = nextMatrixLines wordChars edgeWord prevLine index

-- generate the lines of a Wagner-Fischer matrix
firstMatrixLine :: WordChars -- word to search
                -> MatrixLine -- first line

firstMatrixLine wordChars =
    array (0, wordLength) (map (\i -> (i,i)) [0..wordLength])
        where
            wordBounds = bounds wordChars
            wordLength = snd wordBounds - fst wordBounds + 1
nextMatrixLines :: WordChars -- word to search
               -> Word -- edge's current word
               -> MatrixLine -- prev line
               -> Int -- new line index
               -> MatrixLine -- new line
nextMatrixLines wordChars edgeChars prevLine nextLineIdx =
  fst $ foldl f (prevLine, nextLineIdx) edgeChars
  where
    f (line,index) char = (newLine, index+1)
      where newLine = nextMatrixLine wordChars char line index

nextMatrixLine :: WordChars -- word to search
               -> Char -- edge's current char
               -> MatrixLine -- prev line
               -> Int -- new line index
               -> MatrixLine -- new line
nextMatrixLine wordChars edgeChar prevLine nextLineIdx =
  array (0,wordLength) nextLine
    where
        wordBounds = bounds wordChars
        wordLength = snd wordBounds - fst wordBounds + 1
        nextLineEl i prev
            | i == 0                        = (prevLine ! 0) + 1
            | wordChars ! (i-1) == edgeChar = prevLine ! (i-1)
            | otherwise                     = 1 + min3 (prevLine ! (i-1))
                                                       (prevLine ! i)
                                                       prev
        nextLine = foldl (\currArr i -> currArr ++ [(i,nextLineEl i (if i == 0 then 0 else snd $ last currArr))] ) [] [0..wordLength]


min3 :: Ord a => a -> a -> a -> a
min3 x y z = min z $ min x y