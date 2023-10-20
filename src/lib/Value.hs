module Value (Value (..), Record (..))
where

import Data.Map (Map)

-- a value is either an integer, a string, a list of values or a record
data Value = VInt Int | VString String | VList [Value] | VRecord Record
    deriving (Show, Eq)

-- a record is a map of values indexed by strings
newtype Record = Record (Map String Value)
    deriving (Show, Eq)
