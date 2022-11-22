-- define user data types

-- automatic derivation rules:
-- Eq: two values are equal iff they are built from the same constructor, otherwise they are not equal
-- Enum: the enumeration follows the order in which constructors are declared
-- Bounded: follows the declaration order of constructors, the first being the minimum value and the last being the maximum value
-- Show: print constructors literally

-- TNone means the void turn
-- TAround means a 180 degree turn 
data Turn = TNone | TLeft | TRight | TAround
 deriving (Eq, Enum, Bounded, Show)

data Direction = North | East | South | West
 deriving (Eq, Enum, Bounded, Show)

-- define a type class that represents types whose values can be enumerated in a cycle
-- type constraint: to be a CyclicEnum a class must be an instance of Eq, Bounded and Enum
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
 cpred :: a -> a
 cpred d
  | d == minBound = maxBound
  | otherwise = pred d

 csucc :: a -> a
 csucc d
  | d == maxBound = minBound
  | otherwise = succ d

-- declare that a Direction is an instance of CyclicEnum
instance CyclicEnum Direction
