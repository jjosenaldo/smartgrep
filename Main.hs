module Main where
import Search (searchTree, SearchResult (distance, sr_word, sr_occurrences), getClosestResults, buildTextFromClosestResults)
import OrderedList (toList)
import TreeBuilder (buildTree)
import System.Environment (getArgs)
import Data.Array (elems)

main :: IO ()
main = do
        args <- getArgs
        let word = getWordFromArgs args
        tree <- buildTree
        let allResults = toList $ searchTree tree word
        let resultText = buildTextFromClosestResults allResults
        putStr resultText

getWordFromArgs :: [String] -> String
getWordFromArgs args =
        case args of
                [] -> error "You must provide a word to search, like so: \"./Main horse\""
                [x] -> x
                _:_:_ -> error "You must provide a single word"

