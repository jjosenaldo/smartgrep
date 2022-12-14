module Map (
    Map,
    set,
    get,
    newMap,
    emptyMap,
    entries,
    values,
    mergeMap,
    isMapEmpty
) where

newtype Map a b = Map [(a,b)]

entries :: Map a b -> [(a,b)]
entries (Map xs) = xs

values :: Map a b -> [b]
values (Map xs) = map snd xs

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

isMapEmpty :: Map a b -> Bool
isMapEmpty m  = null $ entries m 

mergeMap :: Eq a => (a -> b -> b -> b) -- conflict resolution
                 -> Map a b -- first map
                 -> Map a b -- second map
                 -> Map a b -- result map
mergeMap conflictSolver m1 m2  = foldr mergeAcc  m1 (entries m2)
    where
        mergeAcc (key,val) acc = 
            case maybeExistingVal of
                Nothing -> set acc key val
                Just existingVal -> set acc key (conflictSolver key existingVal val) 
                where maybeExistingVal = get acc key
