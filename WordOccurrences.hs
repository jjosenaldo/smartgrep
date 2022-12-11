module WordOccurrences where
import Map (Map, mergeMap, isMapEmpty, entries)
import Prelude hiding (Word)
import Word (Word)

newtype WordOccurrences = WordOccurrences {linesTimes :: Map String (Map Int Int)}
type WordOccurrencesByWord = Map Word WordOccurrences

instance Show WordOccurrences where
    show (WordOccurrences linesTimes) = concatMap forFile (entries linesTimes)
        where
            forFile (file,linesMap) = concatMap forLine (entries linesMap)
                where forLine (line, times) = file ++ ":" ++ show line ++ (if times == 1 then "" else " (" ++ show times ++ "x)") ++ "\n"

isOccurrencesEmpty :: WordOccurrences -> Bool
isOccurrencesEmpty (WordOccurrences lines) = isMapEmpty lines


addOccurrencesByWord :: WordOccurrencesByWord -> WordOccurrencesByWord -> WordOccurrencesByWord
addOccurrencesByWord = mergeMap conflictSolver
    where
        conflictSolver _ = addOccurrences

addOccurrences :: WordOccurrences -> WordOccurrences -> WordOccurrences
addOccurrences occs1 occs2 = WordOccurrences $ mergeMap conflictSolver (linesTimes occs1) (linesTimes occs2)
    where
        conflictSolver _ = addLines

addLines :: Map Int Int -> Map Int Int -> Map Int Int
addLines = mergeMap (\_ _ v2 -> v2)
