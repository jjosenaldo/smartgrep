module Map (
    Map,
    set,
    get,
    newMap,
    emptyMap,
    entries
) where

newtype Map a b = Map [(a,b)]

entries :: Map a b -> [(a,b)]
entries (Map xs) = xs

set :: Eq a => Map a b -> a -> b -> Map a b
set (Map []) k v = Map [(k,v)]
set (Map (x:xs)) k v = if fst x == k then Map ((k,v):xs) else Map (x:ys)
    where Map ys = set (Map xs) k v

get :: Eq a => Map a b -> a -> Maybe b
get (Map []) _ = Nothing
get (Map (x:xs)) k = if fst x == k then Just $ snd x else get (Map xs) k

newMap :: a -> b -> Map a b 
newMap k v = Map [(k,v)] 

emptyMap :: Map a b 
emptyMap = Map []