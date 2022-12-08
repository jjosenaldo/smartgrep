module Main where
import Data.Array (listArray)
import Search (searchTree)
import WordTree (t5)
import Data.Array.Base (elems)
import OrderedList (toList)

main = do
        let wordToSearch = listArray (0,4) "testa"
        let result =  searchTree t5 wordToSearch
        print $ toList result
        return ()