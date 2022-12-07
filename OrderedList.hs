module OrderedList where

data OrderedList a = OrderedList [a]

emptyList :: OrderedList a
emptyList = OrderedList []

insertAtList :: Ord a => [a] -> a -> [a]
insertAtList [] x = [x]
insertAtList (x:xs) y = if x <= y then x : insertAtList xs y else x:y:xs

concatList :: Ord a => [a] -> [a] -> [a]
concatList = foldr (flip insertAtList)