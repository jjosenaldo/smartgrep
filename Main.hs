module Main where
import Search (searchTree)
import OrderedList (toList)
import TreeBuilder (buildTree)
import System.Environment (getArgs)

main = do
        args <- getArgs
        tree <- buildTree
        let word
             | null args = error "You must provide a word to search, like so: \"./Main horse\""
             | otherwise = 
                case args of 
                    [x] -> x
                    _ -> error "You must provide a single word"
        let resultList = toList $ searchTree tree word
        print $ take 5 resultList
        return ()