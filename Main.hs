module Main where
import Search (searchTree)
import OrderedList (toList)
import TreeBuilder (buildTree)
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        let word =
                case args of 
                    [] -> error "You must provide a word to search, like so: \"./Main horse\""
                    [x] -> x
                    _:_:_ -> error "You must provide a single word"
        tree <- buildTree
        let resultList = toList $ searchTree tree word
        print $ take 5 resultList
        return ()