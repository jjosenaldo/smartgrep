module Search where
import Word (Word, isEmpty, PrefixResult (..), commonPrefix, first)
import Prelude hiding (Word)
import WordTree (WordOccurrence, WT (..), WTNode (..), WTEdge, isFinal)
import Map (get, entries, values)
import OrderedList (OrderedList)
import Data.Array ( Array, bounds, (!) )
import Data.Array.Base (array)

type RegArray = Array Int
type MatrixLine = RegArray Int
type WordChars = RegArray Char

data SearchResult = SearchResult {
    sr_word :: Word,
    distance :: Int,
    sr_occurrences :: [WordOccurrence]
}

instance Eq SearchResult where
  res1 == res2 = distance res1 == distance res2

instance Ord SearchResult where
  res1 <= res2 = distance res1 <= distance res2

-- searchNode :: WTNode    -- current node
--            -> WordChars -- current word
--            -> MatrixLine -- prev matrix line
--            -> OrderedList SearchResult -- prev results
--            -> (OrderedList SearchResult,MatrixLine) -- new results, next matrix line
-- searchNode node chars prevLine prevResults = 

-- generate the lines of a Wagner-Fischer matrix
firstMatrixLine :: WordChars -- word to search
                -> MatrixLine -- first line
firstMatrixLine wordChars =
    array (0, wordLength-1) (map (\i -> (i,i)) [0..])
        where
            wordBounds = bounds wordChars
            wordLength = snd wordBounds - fst wordBounds + 1
nextMatrixLine :: WordChars -- word to search
               -> Char -- edge's current char
               -> MatrixLine -- prev line
               -> Int -- new line index
               -> MatrixLine -- new line
nextMatrixLine wordChars edgeChar prevLine nextLineIdx =
    if nextLineIdx == 0 then array (0,wordLength-1) $ map (\i -> (i, i + nextLineIdx)) [0..wordLength-1]
    else array (0,wordLength-1) nextLine
    where
        wordBounds = bounds wordChars
        wordLength = snd wordBounds - fst wordBounds + 1
        nextLineEl i prev
            | i == 0                        = (prevLine ! 0) + 1
            | wordChars ! i == edgeChar = prevLine ! (i-1)
            | otherwise                     = 1 + min3 (prevLine ! (i-1))
                                                       (prevLine ! i)
                                                       prev
        nextLine = foldl (\currArr i -> currArr ++ [(i,nextLineEl i (if i == 0 then 0 else snd $ last currArr))] ) [] [0..wordLength-1]


min3 :: Ord a => a -> a -> a -> a
min3 x y z = min z $ min x y