module OrderedList where

newtype OrderedList a = OrderedList [a]

instance Functor OrderedList where 
    fmap f (OrderedList xs) = OrderedList $ map f xs

instance Foldable OrderedList where
  foldr f b (OrderedList xs) = foldr f b xs

instance Show a => Show (OrderedList a) where 
  show (OrderedList xs) = show xs

emptyList :: OrderedList a
emptyList = OrderedList []

insertAtList :: Ord a => OrderedList a -> a -> OrderedList a
insertAtList (OrderedList []) x = OrderedList [x]
insertAtList (OrderedList (x:xs)) y = if x <= y then OrderedList(x : toList (insertAtList (OrderedList xs) y)) else OrderedList(y:x:xs)

concatList :: Ord a => OrderedList a -> OrderedList a -> OrderedList a
concatList = foldr (flip insertAtList)

toList :: OrderedList a -> [a]
toList (OrderedList xs) = xs