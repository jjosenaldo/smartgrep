{-# LANGUAGE QuasiQuotes #-}
module Main where
import Search (searchTree, SearchResult (distance, sr_word, sr_occurrences), getClosestResults, buildTextFromClosestResults)
import OrderedList (toList)
import TreeBuilder (buildTree)
import System.Environment (getArgs)
import Data.Array (elems)
import System.Console.Docopt
import Text.Read (readMaybe)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
        args <- parseArgsOrExit patterns =<< getArgs
        word <- args `getArgOrExit` argument "word"
        maxDistance <- intOption args "distance" "The max distance"
        maxSuggestions <- intOption args "suggestions" "The max suggestions"

        tree <- buildTree
        let allResults = toList $ searchTree tree word
        let resultText = buildTextFromClosestResults maxDistance maxSuggestions allResults 
        putStr resultText


intOption :: Arguments -> String -> String -> IO Int
intOption args optionName optionDesc = 
        do 
                valStr <- args `getArgOrExit` longOption optionName
                let maybeVal = readMaybe valStr :: Maybe Int
                return (case maybeVal of 
                        Nothing -> error $ optionDesc ++ " should be an integer!"
                        Just val -> val)
