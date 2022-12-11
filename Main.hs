module Main where
import Search (searchTree, SearchResult (distance))
import OrderedList (toList)
import TreeBuilder (buildTree)
import System.Environment (getArgs)

main :: IO ()
main = do
        let maxDistance = 2
        args <- getArgs
        let word =
                case args of 
                    [] -> error "You must provide a word to search, like so: \"./Main horse\""
                    [x] -> x
                    _:_:_ -> error "You must provide a single word"
        tree <- buildTree
        let allResults = toList $ searchTree tree word
        print $ takeWhile (\result -> distance result <= maxDistance) allResults
        return ()