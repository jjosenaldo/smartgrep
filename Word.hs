module Word where

import Prelude hiding (Word)
import Data.Array (Array, array)

type Word = String

wordToArray :: Word -> Array Int Char
wordToArray word = array (0, size word - 1) (map (\i -> (i, charAt word i)) [0..size word - 1])

emptyWord :: Word 
emptyWord = []

size :: Word -> Int 
size = length

charAt :: Word -> Int -> Char 
charAt = (!!)

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