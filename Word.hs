module Word where

import Prelude hiding (Word)

type Word = String

emptyWord :: Word 
emptyWord = []

first :: Word -> Char
first = head

isEmpty :: Word -> Bool
isEmpty = null

concatWord :: Word -> Word -> Word
concatWord = (++)

commonPrefix :: Word -> Word -> PrefixResult
commonPrefix xs@[] ys = PrefixResult {prPrefix=[], prRemainder1=xs, prRemainder2=ys}
commonPrefix xs ys@[] = PrefixResult {prPrefix=[], prRemainder1=xs, prRemainder2=ys}
commonPrefix xs'@(x:xs) ys'@(y:ys) = 
    if x /= y 
        then PrefixResult {prPrefix=[], prRemainder1=xs', prRemainder2=ys'}
        else PrefixResult {prPrefix=x:prPrefix result, prRemainder1=prRemainder1 result, prRemainder2=prRemainder2 result}
            where 
                result = commonPrefix xs ys   

data PrefixResult = PrefixResult {
    prPrefix :: Word,
    prRemainder1 :: Word,
    prRemainder2 :: Word
} deriving (Show)