module Main where
import Search (searchTree)
import OrderedList (toList)
import TreeBuilder (buildTree)

main = do
        tree <- buildTree
        let resultList = toList $ searchTree tree "angustia"
        print $ take 5 resultList
        return ()