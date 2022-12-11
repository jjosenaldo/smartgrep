module TreeBuilder where

import Map (Map, emptyMap, set, get, entries, mergeMap)
import WordTree (WT, insert, emptyTree, emptyOccurrences)
import Prelude hiding (Word)
import Word (Word)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import Data.Functor ( (<&>) )
import WordOccurrences (WordOccurrencesByWord, WordOccurrences (WordOccurrences, linesTimes), addOccurrencesByWord)
import Data.List  (isPrefixOf)
import Data.Char (toLower, isLetter)

buildTree :: IO WT
buildTree = (getFiles >>= getOccurrencesFromFiles) <&> buildTreeFromOccurrences

filesDir :: FilePath 
filesDir = "files"

getFiles :: IO [FilePath]
getFiles = ((getCurrentDirectory >>= (\dir -> pure $ dir ++ '/':filesDir)) >>= getDirectoryContents ) <&> map ((filesDir ++ "/") ++). filter (not . isPrefixOf ".")

buildTreeFromOccurrences :: WordOccurrencesByWord -> WT
buildTreeFromOccurrences occurrences = foldr foldAcc emptyTree (entries occurrences)
    where 
        foldAcc :: (Word, WordOccurrences) -> WT -> WT
        foldAcc (word, occurrences) treeAcc = insert treeAcc word occurrences

getOccurrencesFromFiles :: [FilePath]
                           -> IO WordOccurrencesByWord
getOccurrencesFromFiles = foldr mapAcc (pure emptyMap)
            where
                mapAcc :: FilePath -> IO WordOccurrencesByWord -> IO WordOccurrencesByWord
                mapAcc file accIO =
                    do
                        acc <- accIO
                        occsFromFile <- getOccurrencesFromFile file
                        return $ addOccurrencesByWord acc occsFromFile

getOccurrencesFromFile :: FilePath
                       -> IO WordOccurrencesByWord
getOccurrencesFromFile file =
    do
        fileContent <- readFile file
        let fileLines = lines fileContent
        let occurrences =
             foldrIndexed mapAcc emptyMap fileLines
              where
                mapAcc :: Int -> String -> WordOccurrencesByWord -> WordOccurrencesByWord
                mapAcc index line acc =
                    addOccurrencesByWord occurrencesFromLine acc
                    where
                        occurrencesFromLine = getOccurrencesFromLine line (index+1) file
        return occurrences


getOccurrencesFromLine :: String  -- line
                       -> Int -- lineNumber
                       -> String  -- fileName
                       -> WordOccurrencesByWord
getOccurrencesFromLine line lineNumber file =
    foldr mapAcc emptyMap lineWords
    where
        lineWords = map preproccessWord $ words line
        mapAcc :: String -> WordOccurrencesByWord -> WordOccurrencesByWord
        mapAcc word acc = set acc word newVal
            where
                newVal :: WordOccurrences
                newVal =
                    case get acc word of
                        Nothing -> incOccurrences emptyOccurrences lineNumber file
                        Just occurrences -> incOccurrences occurrences lineNumber file

preproccessWord ::  String -> String
preproccessWord = filter isLetter . map toLower

foldrIndexed :: (Int -> a -> b -> b) -> b -> [a] -> b
foldrIndexed f b xs = fst $ foldr (\x (acc, index) -> (f index x acc, index + 1)) (b, 0) xs

incOccurrences :: WordOccurrences -- current occurrences
              -> Int -- line number
              -> String -- fileName
              -> WordOccurrences -- new occurrences
incOccurrences currOccurrences line file =
    case maybeOccurrencesInFile of
        Nothing -> WordOccurrences {linesTimes = set (linesTimes currOccurrences) file (set emptyMap line 1)}
        Just occurrencesInFile ->
            case maybeOccurrencesInLine of
                Nothing -> WordOccurrences {linesTimes = set (linesTimes currOccurrences) file (set occurrencesInFile line 1)}
                Just occurrencesInLine -> WordOccurrences {linesTimes = set (linesTimes currOccurrences) file (set occurrencesInFile line (occurrencesInLine+1))}
                where maybeOccurrencesInLine = get occurrencesInFile line

        where
            maybeOccurrencesInFile = get (linesTimes currOccurrences) file